module TwitterClient  where
import Network.Browser
import Network.HTTP hiding (user)
import Text.XML.HaXml
import Text.XML.HaXml.Pretty
import Codec.Binary.UTF8.String

data TwitterStatus = Status {user :: User, status :: String, statusId :: Integer} deriving Show
data User = User { screenName :: String, name :: String, userId :: Integer } deriving Show

tweet user pass stat = browse $ tweeter user pass stat

getText filt target = concatMap (maybe "" id . fst)  $ (textlabelled filt) target

getFriendsTimeLine user pass args = do
  xml <- browse $ getTimeLine "friends" user pass args
  let Document _ _ tmp _ = xmlParse "timeline" xml
      elems = CElem $ xmlUnEscape stdXmlEscaper tmp
      in return $ map buildMsg $ (tag "statuses" /> tag "status") elems
  where buildMsg conts =
          let usrTag = head $ (tag "status" /> tag "user") conts
              scname = getText (deepest $ tag "screen_name" /> txt) usrTag
              uname = getText (deepest $ tag "name" /> txt) usrTag
              uid   = read $ getText (deepest $ tag "id" /> txt) usrTag
              usr = User {screenName = scname, name = uname, userId = uid}
              text = getText (tag "status" /> tag "text" /> txt) conts
              sid  = read $ getText (tag "status" /> tag "id" /> txt) conts
              in Status {user=usr, status=text, statusId=sid}

getTimeLine kind user pass args = do 
  setAuthorityGen (const.const(return $ Just (user, pass)))
  setOutHandler (const $ return ())
  (_,res) <- request $ getRequest ("http://twitter.com/statuses/"++kind++"_timeline.xml?" ++ urlEncodeVars args)
  return $ rspBody $ res

tweeter user pass stat = do
  setAuthorityGen (const.const(return $ Just (user, pass)))
  setOutHandler (const $ return())
  request $ postRequest ("http://twitter.com/statuses/update.xml?" ++ urlEncodeVars [("status", stat)] )

