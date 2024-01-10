module BlogHs.Convert where

import qualified BlogHs.Html as Html
import qualified BlogHs.Markup as Markup

-- qualified imports are imports that needs to be called with prefixed Html.Document

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    -- Markup.Heading 1 txt ->
    --     Html.h1_ text

    Markup.Heading n txt ->
      Html.h_ n txt
    Markup.Paragraph p ->
      Html.p_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list
    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list
    Markup.CodeBlock list ->
      Html.code_ (unlines list)

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure