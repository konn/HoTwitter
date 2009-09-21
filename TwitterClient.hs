module TwitterClient  where
import Network.Browser
import Network.HTTP hiding (user)
import Text.XML.HaXml
import Text.XML.HaXml.Escape
import Text.XML.HaXml.Pretty
import Codec.Binary.UTF8.String

data TwitterStatus = Status {user :: User, status :: String, statusId :: Integer} deriving Show
data User = User { screenName :: String, name :: String, userId :: Integer } deriving Show

tweet user pass stat = browse $ tweeter user pass stat

getText filt target = concatMap (render.content) $ xmlUnEscapeContent stdXmlEscaper $ (deepest $ filt /> txt) target

getFriendsTimeLine user pass args = do
  xml <- browse $ getTimeLine "friends" user pass args
  let Document _ _ tmp _ = xmlParse "timeline" xml
      elms = CElem tmp undefined
      in return $ map buildMsg $ (tag "statuses" /> tag "status") elms
  where buildMsg conts =
          let usrTag = head $ (tag "status" /> tag "user") conts
              scname = getText (tag "screen_name") usrTag
              uname = getText (tag "name") usrTag
              uid   = read $ getText (deepest $ tag "id") usrTag
              usr = User {screenName = scname, name = uname, userId = uid}
              text = unescapeString $ getText (tag "status" /> tag "text") conts
              sid  = read $ getText (tag "status" /> tag "id") conts
              in Status {user=usr, status=text, statusId=sid}

unescapeString :: String -> String
unescapeString str = let (Document _ _ es _) = xmlParse "" $ "<dummy>" ++ str ++ "</dummy>"
                     in concatMap (render . content) . xmlUnEscapeContent stdXmlEscaper . deepest txt $ (CElem es undefined)


getTimeLine kind user pass args = do 
  setAuthorityGen (const.const(return $ Just (user, pass)))
  setOutHandler (const $ return ())
  (_,res) <- request $ getRequest ("http://twitter.com/statuses/"++kind++"_timeline.xml?" ++ urlEncodeVars args)
  return $ rspBody $ res

tweeter user pass stat = do
  setAuthorityGen (const.const(return $ Just (user, pass)))
  setOutHandler (const $ return())
  request $ postRequest ("http://twitter.com/statuses/update.xml?" ++ urlEncodeVars [("status", stat)] )

