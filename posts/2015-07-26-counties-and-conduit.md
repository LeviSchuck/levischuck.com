---
title: Counties and Conduit
mathjax: true
---

About two years ago, I worked with my grandpa to produce a database of cities (at the latitude and longitude level) and historic counties / state based on GIS files with polygons from [Newberry][].
It wasn't too difficult, I loaded them into [PostGIS][] on a local [PostgreSQL][] instance, joined based on cities being inside a polygon or not, where each polygon had an associated historic county name, state, territory, and date range.

The problem I solved for my grandpa was determining where genealogy records may reside at given locations for periods of time. For example: Joe Jones died in 1890 in Overbrook, New Jersey. Today the county is Camden, but that may not have been the county in 1830, which is Gloucester. With this information, a genealogist can find out that they should look next at the Gloucester records.

At first, I worked on only one US state of information, to see if my query worked properly. It wasn't a simple query to execute, the timing it took felt higher than $O(N*M)$, but then again both $N$ and $M$ were significant in size.

```SQL
insert into cities values('Alabama', 'Baldwin', 'Atkins Mill', ST_GeomFromText('POINT(-87.86333 32.98389)', 26768));
insert into cities values('Alabama', 'Baldwin', 'Barlow Landing', ST_GeomFromText('POINT(-87.8725 30.95722)', 26768));
...
```

*Unfortunately, I cannot find the original query I used on the joined data.*

After producing this information, two years ago, there were a few duplicates. I underestimated just how much.

My Grandfather published one book on his current home state with the information after trudging through all the duplicates and messy details.
But now that the one state is finished, it is time to optimize the process.

Here's one example from Kansas, Oursler.

```c
State, County, City, Historic State, Historic County, Start Date, End Date, lat, long
...
"Kansas","Marion","Oursler",,"Unorganized Federal Territory",5/6/1828 0:00:00,6/27/1834 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler",,"Missouri Territory",2/22/1819 0:00:00,8/9/1821 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","Non-County Area 1",7/4/1819 0:00:00,12/31/1820 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler",,"Unorganized Federal Territory",8/10/1821 0:00:00,12/12/1850 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","ST. LOUIS (Mo.)",10/1/1804 0:00:00,12/6/1812 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler",,"Unorganized Federal Territory",5/26/1824 0:00:00,5/5/1828 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler",,"Unorganized Federal Territory",8/10/1821 0:00:00,5/25/1824 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","Non-County Area 1",1/1/1821 0:00:00,8/9/1821 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler",,"Unorganized Federal Territory",6/28/1834 0:00:00,3/27/1837 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","KS","MARION",8/25/1855 0:00:00,2/16/1860 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","ST. LOUIS (Mo.)",10/1/1804 0:00:00,12/6/1812 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","KS","MARION",3/6/1873 0:00:00,12/31/2000 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","KS","MARION",2/22/1865 0:00:00,2/25/1867 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","St. Louis District",10/1/1804 0:00:00,12/6/1812 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","KS","MARION",2/26/1867 0:00:00,3/2/1868 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","ST. LOUIS (Mo.)",10/1/1804 0:00:00,12/6/1812 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler",,"Unorganized Federal Territory",12/13/1850 0:00:00,5/29/1854 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","KS","MARION",2/17/1860 0:00:00,2/21/1865 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","ST. LOUIS (Mo.)",10/1/1804 0:00:00,12/6/1812 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","ST. LOUIS (Mo.)",10/1/1804 0:00:00,12/6/1812 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","KS","MARION",3/3/1868 0:00:00,3/5/1873 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","KS","Kansas Territory",5/30/1854 0:00:00,8/24/1855 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","Non-County Area 1",12/7/1812 0:00:00,12/30/1813 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","Non-County Area 1",6/1/1819 0:00:00,7/3/1819 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","Non-County Area 1",12/31/1813 0:00:00,2/21/1819 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler",,"Missouri Territory",12/7/1812 0:00:00,2/21/1819 0:00:00,38.29,-96.98
"Kansas","Marion","Oursler","MO","Non-County Area 1",2/22/1819 0:00:00,5/31/1819 0:00:00,38.29,-96.98
```

As you can see, the details that came back from my grandpa's Microsoft Access export are not necessarily in a convenient order. Nor are the dates lexicographically friendly for sorting.

This was my starting point in this chapter of the project.

# Starting point

First, I needed to come up with a sensible representation.
Having everything represented perfectly in Haskell wasn't my goal.
I just needed to do some basic grouping, sorting, and event-sourcing.

My first decision on the data structure was, `ByteString` is fine for everything, so long as they were lexicographically sort-friendly.

I also had no interest in using a complicated [csv-conduit] package where I had doubts of it being stream-frienly on a row basis.
So I wrote my own CSV parser of sorts. It isn't perfect, but it works on the data I need.

```haskell

quote :: Word8
quote = BI.c2w '\"'
comma :: Word8
comma = BI.c2w ','

csvSplit :: (Monad m) => Conduit B.ByteString m [B.ByteString]
csvSplit = do
    v <- await
    case v of
        Nothing -> return ()
        Just v' -> do
            yield $ split v'
            csvSplit
    where
        split v
            | B.null v = []
            | B.head v == comma = B.empty : split (B.drop 1 v)
            | B.head v == quote =
                let str = B.drop 1 v
                    (quoted,rest) = B.break (== quote) str
                    requoted = B.cons quote $ B.snoc quoted quote
                in requoted : split (B.drop 2 rest)
            | otherwise =
                let (part,rest) = B.break (== comma) v
                in  part : split (B.drop 1 rest)
```

Awesome, now I have a list of bytestrings (for each field).
Let's double check that this parses the right amount of fields for the entire file.

```haskell

import qualified Data.Conduit as C
import qualified Data.Conduit.Binary as CB
import qualified Control.Monad.Trans.Resource as R

-- Conduit combinators WOULD NOT WORK in ghci (it can't find it..?)
-- So, I used my own helpers instead (printC)

printC :: (Show a, MonadIO m) => C.Sink a m ()
printC = do
    v <- C.await
    case v of
        Just v' -> do
            liftIO . putStrLn . show $ v'
            printC
        Nothing -> return ()

listCount :: (Monad m) => C.Conduit [a] m Int
listCount = do
    v <- C.await
    case v of
        Nothing -> return ()
        Just v' -> do
            C.yield $ length v'
            listCount

equalLength :: (Monad m, Eq a) => C.Conduit a m (a,Int)
equalLength = el Nothing 0
    where el ov c = do
            v <- C.await
            case (ov,v) of
                (Nothing, Nothing) -> return ()
                (Just x, Nothing) -> C.yield (x, c)
                (Nothing, Just _) -> el v 1
                (Just x1, Just x2) -> if x1 == x2
                    then el ov (c+1)
                    else do
                        C.yield (x1,c)
                        el v 1


main = do
    R.runResourceT
        $ CB.sourceFile "USA Base Unlinked.txt" $= CB.lines
        $= E.csvSplit
        $= listCount
        $= equalLength
        $$ printC

```

And out came something like `(9,400000)`

Good! Now I can move on to putting into my internal representation and mess around with it.

# Simple Data Structure

As I said above, ByteStrings would be good enough for me, though I'll need to convert
this messy date `10/1/1804 0:00:00` to something like `1804-10-01`.

```haskell
data CountyRow = CountyRow
    { state :: B.ByteString
    , city :: B.ByteString
    , county :: B.ByteString
    , coordLat :: B.ByteString
    , coordLong :: B.ByteString
    , startDate :: B.ByteString
    , endDate :: B.ByteString
    , historicState :: B.ByteString
    , historicCounty :: B.ByteString
    } deriving (Eq,Show)

convertOld :: [B.ByteString] -> CountyRow
convertOld bs = CountyRow
        { state = s, city = ci, county = co, coordLat = cla
        , coordLong = clo, startDate = sd, endDate = ed
        , historicState = hs, historicCounty = hc
        }
    where
        s = bs !! 0
        ci = bs !! 2
        co = bs !! 1
        cla = bs !! 7
        clo = bs !! 8
        sd = todate $ bs !! 5
        ed = todate $ bs !! 6
        hc = bs !! 4
        hs = bs !! 3

-- I have the Word8 things in my 'Extras' module qualified by E.
-- ByteString Internals has a nice Char to Word8 fn for stuff like this
space :: Word8
space = BI.c2w ' '
slash :: Word8
slash = BI.c2w '/'
dash :: Word8
dash = BI.c2w '-'
zero :: Word8
zero = BI.c2w '0'

todate :: B.ByteString -> B.ByteString
todate b =
    let (date, _) = B.break (== E.space) b
        parts = B.split E.slash date
        year = parts !! 2
        month = parts !! 0
        day = parts !! 1
        arrange =
            [ year
            , if B.length month < 2
                then B.cons E.zero month
                else month
            , if B.length day < 2
                then B.cons E.zero day
                else day
            ]
        redash = intersperse (B.singleton E.dash) arrange
    in B.concat redash

```

Now that I have my parsing code set up, I can do something like this to
print it out and see if it came out right.

```haskell
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Control.Monad.Trans.Resource as R

main = do
    R.runResourceT
        $ CB.sourceFile "USA Base Unlinked.txt" $= CB.lines
        $= E.csvSplit $= CL.map convertOld
        $$ printC
```

And indeed I got a nice representation out!

# Reordering the data

However, because the data is partially sorted, and I know it is partially sorted,
I can pause and reorder the stream so each row is friendly for doing some quasi-event sourcing.

At first I wrote the code specific to a a given sorting and comparison, but I found Conduit
amazing at refactoring and generalization.

```haskell
collect :: (Monad m) => (a -> a -> Bool) ->  Conduit a m [a]
collect f = c Nothing []
    where c ov s = do
            v <- await
            case (ov, v) of
                (Nothing, Nothing) -> return ()
                (Just _, Nothing) -> yield s
                (Nothing, Just x) -> c v [x]
                (Just x1, Just x2) -> if f x1 x2
                    then c ov (x2:s)
                    else do
                        yield s
                        c v [x2]

implode :: (Monad m) => Conduit [a] m a
implode = do
    v <- await
    case v of
        Nothing -> return ()
        Just v' -> do
            M.mapM yield v'
            implode

collectAndSort  :: (Monad m)
                => (a -> a -> Bool)
                -> (a -> a -> Ordering)
                -> Conduit a m a
collectAndSort f c = collect f $= CL.map (sortBy c) $= implode

cityKey :: CountyRow -> [B.ByteString]
cityKey cr =
    [ state cr
    , city cr
    , coordLat cr
    , coordLong cr
    ]

compareNormal :: CountyRow -> CountyRow -> Ordering
compareNormal c1 c2
    | state c1 < state c2 = LT
    | state c1 > state c1 = GT
    | city c1 < city c2 = LT
    | city c1 > city c2 = GT
    | county c1 < county c2 = LT
    | county c1 > county c2 = GT
    | coordLat c1 < coordLat c2 = LT
    | coordLat c1 > coordLat c2 = GT
    | coordLong c1 < coordLong c2 = LT
    | coordLong c1 > coordLong c2 = GT
    | startDate c1 < startDate c2 = LT
    | startDate c1 > startDate c2 = GT
    | endDate c1 < endDate c2 = LT
    | endDate c1 > endDate c2 = GT
    | historicState c1 < historicState c2 = LT
    | historicState c1 > historicState c2 = GT
    | historicCounty c1 < historicCounty c2 = LT
    | historicCounty c1 > historicCounty c2 = GT
    | otherwise = EQ
```

Now, I can use something like `$= collectAndSort sameCity compareNormal`
to reorder the stream into a manner convenient for processing.

# Merging

With the data in an event sourcing friendly format, I can have a somewhat
stateful conduit (via tail call recursion) to merge information.

Again, I found Conduit amazingly friendly for refactoring and generalization.
The concept of merging mostly needs to answer "Is it mergeable?" and "How do to merge?"

```haskell
merge   :: (Monad m)
        => (a -> a -> Bool)
        -> (a -> a -> a)
        -> Conduit a m a
merge comp f = m Nothing
    where m ov = do
            v <- await
            case (ov, v) of
                (Nothing, Nothing) -> return ()
                (Just x, Nothing) -> yield x
                (Nothing, Just x) -> m v
                (Just x1, Just x2) -> do
                    if comp x1 x2
                        then m $ Just $ f x1 x2
                        else yield x1 >> m v

mergeId :: (Monad m, Eq a) => Conduit a m a
mergeId = merge (==) (\v _ -> v)

historyKey :: CountyRow -> [B.ByteString]
historyKey cr =
    [ historicCounty cr
    , historicState cr
    ]

mergeCounties :: (Monad m) => C.Conduit CountyRow m CountyRow
mergeCounties = E.merge
    (\x1 x2 -> historyKey x1 == historyKey x2)
    (\x1 x2 ->
        let end = max (endDate x1) (endDate x2)
            start = min (startDate x1) (startDate x2)
        in x2 {startDate = start, endDate = end})
```

Turns out that the data I had was adequately sorted, but not the best,
since city names weren't in perfect order.
Luckily, Conduit being generalization friendly made it so I could reorder the entire
state before processing! Though this added some time and memory to processing.

So I started making my own sorted version.

```haskell
convert :: [B.ByteString] -> CountyRow
convert bs = CountyRow
        { state = bs !! 0, city = bs !! 1, county = bs !! 2
        , coordLat = bs !! 3, coordLong = bs !! 4
        , startDate = bs !! 5, endDate = bs !! 6
        , historicState = bs !! 7, historicCounty = bs !! 8
        }

sameState :: CountyRow -> CountyRow -> Bool
sameState r1 r2 = state r1 == state r2

oldSrc :: (MonadIO m, R.MonadResource m) => C.Source m CountyRow
oldSrc = countiesOld $= convertingOld $= collectState
    where
        countiesOld = CB.sourceFile "USA Base Unlinked.txt" $= CB.lines
        convertingOld = E.csvSplit $= CL.map convertOld
        collectState = E.collectAndSort sameState compareNormal


newSrc :: (MonadIO m, R.MonadResource m) => C.Source m CountyRow
newSrc = counties $= converting
    where
        counties = CB.sourceFile "Ordered USA.csv" $= CB.lines
        converting = E.csvSplit $= CL.map convert

toCSV = CL.map (B.concat . intersperse (B.singleton E.comma) . unconvert)
withLines = CL.map (\v -> B.append v "\n")

sameCity :: CountyRow -> CountyRow -> Bool
sameCity r1 r2 = cityKey r1 == cityKey r2

main1 = do
    R.runResourceT
        $  oldSrc
        $ E.collectAndSort sameCity compareNormal
        $= toCSV $= withLines
        $$ CB.sinkFile "Ordered USA.csv"

main2 = do
    R.runResourceT
        $  newSrc
        -- Remove line duplicates
        $= E.mergeId
        -- Merge counties, keeping earliest and latest
        -- start and end dates respectively
        $= mergeCounties
        $= toCSV $= withLines
        $$ CB.sinkFile "Merged USA.csv"
```

So! Given that I first order things appropriately, now Oursler looks like

```c
"Kansas","Oursler","Marion",38.29,-96.98,1804-10-01,1812-12-06,"MO","ST. LOUIS (Mo.)"
"Kansas","Oursler","Marion",38.29,-96.98,1804-10-01,1812-12-06,"MO","ST. LOUIS (Mo.)"
"Kansas","Oursler","Marion",38.29,-96.98,1804-10-01,1812-12-06,"MO","ST. LOUIS (Mo.)"
"Kansas","Oursler","Marion",38.29,-96.98,1804-10-01,1812-12-06,"MO","ST. LOUIS (Mo.)"
"Kansas","Oursler","Marion",38.29,-96.98,1804-10-01,1812-12-06,"MO","ST. LOUIS (Mo.)"
"Kansas","Oursler","Marion",38.29,-96.98,1804-10-01,1812-12-06,"MO","St. Louis District"
"Kansas","Oursler","Marion",38.29,-96.98,1812-12-07,1813-12-30,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1812-12-07,1819-02-21,,"Missouri Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1813-12-31,1819-02-21,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1819-02-22,1819-05-31,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1819-02-22,1821-08-09,,"Missouri Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1819-06-01,1819-07-03,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1819-07-04,1820-12-31,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1821-01-01,1821-08-09,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1821-08-10,1824-05-25,,"Unorganized Federal Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1821-08-10,1850-12-12,,"Unorganized Federal Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1824-05-26,1828-05-05,,"Unorganized Federal Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1828-05-06,1834-06-27,,"Unorganized Federal Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1834-06-28,1837-03-27,,"Unorganized Federal Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1850-12-13,1854-05-29,,"Unorganized Federal Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1854-05-30,1855-08-24,"KS","Kansas Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1855-08-25,1860-02-16,"KS","MARION"
"Kansas","Oursler","Marion",38.29,-96.98,1860-02-17,1865-02-21,"KS","MARION"
"Kansas","Oursler","Marion",38.29,-96.98,1865-02-22,1867-02-25,"KS","MARION"
"Kansas","Oursler","Marion",38.29,-96.98,1867-02-26,1868-03-02,"KS","MARION"
"Kansas","Oursler","Marion",38.29,-96.98,1868-03-03,1873-03-05,"KS","MARION"
"Kansas","Oursler","Marion",38.29,-96.98,1873-03-06,2000-12-31,"KS","MARION"
```

Very neat!
Now let's see how it looks when I inspect the merged output.

```c
"Kansas","Oursler","Marion",38.29,-96.98,1804-10-01,1812-12-06,"MO","ST. LOUIS (Mo.)"
"Kansas","Oursler","Marion",38.29,-96.98,1804-10-01,1812-12-06,"MO","St. Louis District"
"Kansas","Oursler","Marion",38.29,-96.98,1812-12-07,1813-12-30,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1812-12-07,1819-02-21,,"Missouri Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1813-12-31,1819-05-31,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1819-02-22,1821-08-09,,"Missouri Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1819-06-01,1821-08-09,"MO","Non-County Area 1"
"Kansas","Oursler","Marion",38.29,-96.98,1821-08-10,1854-05-29,,"Unorganized Federal Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1854-05-30,1855-08-24,"KS","Kansas Territory"
"Kansas","Oursler","Marion",38.29,-96.98,1855-08-25,2000-12-31,"KS","MARION"
```

Oh yes!

This is what I wanted! Though there are some duplicates with the same dates, but different names.
Since this is a domain decision, I'll defer how to deal with that to my grandpa for now.

# Conclusion

While working on this project, I recalled how Erlang passes state down for future messages.
It may seem obvious in a functional programming world, but once it clicked here, it became
very easy to handle how Conduit answers `await`.

Let's review my merge function.

```haskell
merge :: (Monad m) => (a -> a -> Bool) -> (a -> a -> a) -> Conduit a m a
merge comp f = m Nothing
    where m ov = do
            v <- await
            case (ov, v) of
                (Nothing, Nothing) -> return ()
                (Just x, Nothing) -> yield x
                (Nothing, Just x) -> m v
                (Just x1, Just x2) -> do
                    if comp x1 x2
                        then m $ Just $ f x1 x2
                        else yield x1 >> m v
```

`merge` is really just a wrapper for a function `m` with an initial state.
At the beginning of the stream `(Nothing, Just x) -> m v`, I just pass along
what the recent value (wrapped in a `Maybe`) is `v`.
At the end of the stream `(Just x, Nothing) -> yield x`, I give up the value I
have merged over time back to the stream.
In the middle, `(Just x1, Just x2) ->`, I do the simple logic of merging or
yielding the previous value when a merge is not appropriate.

I'm not sure how efficient this is from a stream fusion standpoint, though it handled
my needs gracefully.

Also, notice how the functions `comp` and `f` are used, refactoring those out to
be input functions required only 3 lines of change within `merge`, gave me an easy
implementation to drop redundant stream inputs, and also have a domain specific
`mergeCounties` Conduit with an understandable implementation.

Overall, I found the cognitive overhead of 'Conduit' to be amazingly light,
followed the principle of least surprise, and solved my needs.

Although I made the decision of splitting stages into files due to the size and
inequal weight of resorting at the state level, I could have easily just done
something like


```haskell
main3 = do
    R.runResourceT
        $  oldSrc
        $ E.collectAndSort sameCity compareNormal
        -- Remove line duplicates
        $= E.mergeId
        -- Merge counties, keeping earliest and latest
        -- start and end dates respectively
        $= mergeCounties
        $= toCSV $= withLines
        $$ CB.sinkFile "Merged USA.csv"
```

And it would just work.

Overall, I am very pleased with Snoyman's work on [Conduit][].

[Newberry]: http://publications.newberry.org/ahcbp/downloads/
[PostGIS]: http://postgis.net
[PostgreSQL]: http://www.postgresql.org
[csv-conduit]:https://hackage.haskell.org/package/csv-conduit
[conduit]:https://hackage.haskell.org/package/conduit
