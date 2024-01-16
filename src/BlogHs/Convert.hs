module BlogHs.Convert where

import qualified BlogHs.Html as Html
import qualified BlogHs.Markup as Markup

-- qualified imports are imports that needs to be called with prefixed Html.Document

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n $ Html.txt_ txt
    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list
    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ list . Html.txt_) list
    Markup.CodeBlock list ->
      Html.code_ (unlines list)
