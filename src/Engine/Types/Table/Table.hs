module Engine.Types.Table.Table
( Row(..)
, Table(..)
, TableName
, tableProduct
, decodeTable
) where

import           Control.Monad               (join)
import           Control.Monad.Extra         (ifM)
import           Data.Binary
import           Data.Binary.Get             (isEmpty)
import qualified Data.ByteString.Lazy        as BL

import           Engine.Types.Table.PolyType

newtype Row = Row { unRow :: [PolyType] } deriving (Eq, Ord)

instance Show Row where
    show (Row values) = show values

type TableName = String

data Table =
    Table { tableTypes  :: [(String, AType)]
          , primaryKeys :: [String]
          , tableRows   :: [Row]
          } deriving Eq

tableProduct :: [(String, Table)] -> Table
tableProduct tables =
    Table { tableTypes = tables >>= \(name, table) ->
              (map ((++) (name ++ ".") . fst) $ tableTypes table) `zip` (snd <$> tableTypes table)

          , primaryKeys = tables >>= (\(name, table) -> ((++) $ name ++ ".") <$> primaryKeys table)

          , tableRows = Row <$> foldl (\xs ys -> [x ++ y | x <- xs, y <- ys])
                                      [[]]
                                      (map unRow . tableRows . snd <$> tables)
          }

decodeTable :: BL.ByteString -> Table
decodeTable = decode

instance Show Table where
    show (Table types pKeys values) =
           ("Primary keys: " ++ show pKeys ++ "\n")
        ++ (show types)
        ++ (foldl ((++) . (++ "\n")) "" $ map (show . unRow) values)

instance Binary Table where
    put (Table types pKeys values) = do
        put types
        put pKeys
        foldl (>>) mempty $ foldl (>>) mempty . map put . unRow <$> values

    get = do types <- get
             pKeys <- get
             rows  <- getTableData $ snd <$> types
             pure $ Table types pKeys rows

        where getTableData :: [AType] -> Get [Row]
              getTableData types =
                  ifM isEmpty (pure []) $ (:) <$> getRowData types <*> getTableData types

              getRowData :: [AType] -> Get Row
              getRowData types = Row <$> mapM getPolyType types

              getPolyType :: AType -> Get PolyType
              getPolyType BoolType   = PolyBool   <$> get
              getPolyType IntType    = PolyInt    <$> get
              getPolyType FloatType  = PolyFloat  <$> get
              getPolyType StringType = PolyString <$> get
