module SqLite where

import Utils
import Database.HDBC
import Database.HDBC.Sqlite3
import IndexedDb

dir x = concat 
        ["/otherdisk2/data/sqliteDatabases/",x,".db"]

dbConn db = connectSqlite3 db
sql db statement params = do
  conn <- dbConn db
  preparedStatement <- prepare conn statement
  execute preparedStatement params
  commit conn
  results <- fetchAllRows' preparedStatement  
  disconnect conn
  return results

------ indexed key value database

sqlSelectKey = 
  "select value from key_value where key = ?"
sqlInsertKey = 
  "insert into key_value values (?,?)"
sqlDeleteKey = 
  "delete from key_value where key = ?"  
sqlCreateTable = 
  "create table key_value(key text,value text)"
sqlCreateIndex = 
  "create index key_value_index on key_value (key)"

deleteIndexedDb fname = rmFile fname
renameIndexedDb from to = mvFile from to
copyIndexedDb from to = cpFile from to
createIndexedDb fname = do
  conn <- dbConn fname
  createTable <- prepare conn sqlCreateTable
  execute createTable []
  createIndex <- prepare conn sqlCreateIndex
  execute createIndex []
  commit conn
  disconnect conn
  return DbCreated

delete db key = do
  conn <- dbConn db
  selectKey <- prepare conn sqlSelectKey
  execute selectKey [toSql key]
  keys <- fetchAllRows' selectKey
  case length keys of
    0 -> do
      disconnect conn
      return KeyDoesNotExist
    1 -> do
      deleteKey <- prepare conn sqlDeleteKey
      execute deleteKey [toSql key]
      commit conn
      disconnect conn
      return KeyDeleted
  
get db key = do 
  conn <- dbConn db
  selectKey <- prepare conn sqlSelectKey
  execute selectKey [toSql key]
  keys <- fetchAllRows' selectKey
  case length keys of
    1 -> do
      disconnect conn
      return 
        $ KeyReturned $ fromSql $ head $ head keys
    0 -> do
      disconnect conn
      return KeyDoesNotExist    
    
putIfNotExists db key value = do
  conn <- dbConn db
  selectKey <- prepare conn sqlSelectKey
  execute selectKey [toSql key]
  keys <- fetchAllRows' selectKey
  case length keys of
    0 -> do
      insertKey <- prepare conn sqlInsertKey
      execute insertKey [toSql key,toSql value] 
      commit conn
      disconnect conn
      return KeyAdded 
    1 -> do
      disconnect conn
      return KeyAlreadyExists
                      
putConcat db key value = do
  conn <- dbConn db
  selectKey <- prepare conn sqlSelectKey
  execute selectKey [toSql key]
  keys <- fetchAllRows' selectKey
  print $ length keys
  case length keys of
    0 -> do
      insertKey <- prepare conn sqlInsertKey
      execute insertKey [toSql key,toSql value]
      commit conn
      disconnect conn
      return KeyAdded         
    1 -> do
      deleteKey <- prepare conn sqlDeleteKey
      execute deleteKey [toSql key]
      insertKey <- prepare conn sqlInsertKey      
      execute insertKey 
        [toSql key,
         toSql $ concatRow 
         (fromSql $ head $ head keys) value]
      commit conn
      disconnect conn
      return KeyConcatenated

replaceIfExists db key value = do
  conn <- dbConn db
  selectKey <- prepare conn sqlSelectKey
  execute selectKey [toSql key]
  keys <- fetchAllRows' selectKey
  case length keys of
    0 -> do
      disconnect conn
      return KeyDoesNotExist
    1 -> do      
      deleteKey <- prepare conn sqlDeleteKey
      execute deleteKey [toSql key]
      insertKey <- prepare conn sqlInsertKey
      execute insertKey [toSql key,toSql value]
      commit conn
      disconnect conn
      return KeyReplaced

db1 = "/home/umar/db1.db"

exget = get db1 "allo1"
exconcat = putConcat db1 "allo1" "concat"
exdelete = delete db1 "allo1"        
exput = putIfNotExists db1 "allo1" "v1"
exreplace = 
  replaceIfExists db1 "allo1" "vreplacement"
