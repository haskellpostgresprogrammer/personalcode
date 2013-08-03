module Sql where

import Utils
import Data.List
import Data.List.Split

data DDL =
  CreateTable Name [Field]
  | DropTable Name
  | CreateIndex Name Table Column
  | DropIndex Name
  | AddColumn Table Name SqlType
  | DropColumn Table Name
  | CreateView Name Select
  | DropView Name 
--   | AddConstraint Constraint
--   | DropConstraint Name
  deriving Show
  
data DML =
  Insert Table [ColumnValue]
  | Delete Table Conditions
  | Update Table [ColumnValue] Conditions 
  deriving Show

data ColumnValue = 
  ColumnValue Column Value deriving Show

type Column = String
type Value = String
type Name = String
type Table = String

data Conditions = 
  Conditions [Condition] 
  | NoConditions deriving Show
data Condition = 
  Equals String String
  | GreaterThan String String
  | LessThan String String
  deriving Show

data Field = Field Column SqlType deriving Show

data SqlType = 
  SqlBool | SqlInt | SqlFloat | SqlString 
  | SqlTimeStamp deriving Show

type Index = String
data Limit = NoLimit | Limit Int deriving Show
data Order = 
  Ascending Column 
  | Descending Column 
  | NoOrder deriving Show
data TableColumn = 
  TableColumn Table Column 
  | AllColumns deriving Show

data Select =
  Select [TableColumn] [Table] Conditions 
  Order Limit deriving Show

data LeftJoin = 
  LeftJoin Table Col Table Col [ColAlias]
type ColAlias = (String,String)
type Col = String

comma x = x ++ ","
dot x y = dots [x,y]
leftJoinT (LeftJoin ta ja tb jb cl) = 
  statement 
  ["select",
   commas $ map colAlias cl,
   "from",ta,"left join",tb,"on",
   parens $ spaces [ja,"=",jb]]
  where
    colAlias (x,y) = spaces [x,"as",quoted y]
leftJoin ca cb cl =
  leftJoinT (LeftJoin (st ca) ca (st cb) cb cl)
    where
      st x = dots $ take 2 $ splitOn "." x
-- select t cl = 
--   statement ["select",commas cl,"from",t]
      

selectT (Select tcl tl cs o li) =
  statement 
  ["select",commas $ map tableCol tcl,"from",
   commas tl,conditions cs,order o,limit li]

tableCol (TableColumn t c) = concat [t,".",c]
tableCol (AllColumns) = "*"
order (Ascending c) = spaces ["order by",c,"asc"]
order (Descending c) = spaces ["order by",c,"desc"]
order NoOrder = ""
limit NoLimit = ""
limit (Limit i) = spaces ["limit",show i]

conditions (NoConditions) = ""
conditions (Conditions cl) = 
  statement 
  ["where",intercalate "and" $ map condition cl]
condition (Equals a b) = concat [a," = ",b]
condition (GreaterThan a b) = concat [a," > ",b]
condition (LessThan a b) = concat [a," < ",b]


argList l = parens $ commas l
statement sl = concat [spaces sl,";"]

field (Field c t) = spaces [c,sqlType t]
sqlType SqlBool = "bool"
sqlType SqlInt = "int"
sqlType SqlFloat = "float"
sqlType SqlString = "text"
sqlType SqlTimeStamp = "timestamp"

ddl (CreateTable n fl) = 
  statement
  ["create","table",n,argList $ map field fl]
ddl (DropTable n) = 
  statement ["drop","table",n]
ddl (CreateIndex n t c) =
  statement ["create","index",n,"on",t,parens c]
ddl (DropIndex n) =
  statement ["drop","index",n]
ddl (AddColumn t n s) =
  statement ["alter","table",t,"add",n,sqlType s]
ddl (DropColumn t n) =
  statement ["alter","table",t,"drop","column",n]
ddl (CreateView n s) = 
  statement ["create","view",n,"as",selectT s]
ddl (DropView n) = statement ["drop","view",n]

    
dml (Delete t cs) = 
  statement ["delete","from",t,conditions cs]
dml (Update t cl cs) = 
  statement ["update",t,"set",colsvals cl,conditions cs]
    where
      colsvals l = 
        commas 
        $ map (\(ColumnValue x y) -> concat [x,"=",y]) l
dml (Insert t cl) = 
  statement 
  ["insert","into",t,cols cl,"values",vals cl]
    where
      cols l = 
        argList $ map (\(ColumnValue x y) -> x) l
      vals l = 
        argList $ map (\(ColumnValue x y) -> y) l


