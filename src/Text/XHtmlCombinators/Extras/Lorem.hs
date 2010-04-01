{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.XHtmlCombinators.Extras.Lorem
-- Copyright   : (c) Alasdair Armstrong 2010
-- License     : BSD-style
-- Maintainer  : alasdair.armstrong@googlemail.com
-- Stability   : experimental
-- Portability : GHC

module Text.XHtmlCombinators.Extras.Lorem
    ( lorem', lorem
    , loremIO', loremIO
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Monoid
import System.Random

import Data.Text (Text)
import qualified Data.Text as T

import Text.XHtmlCombinators
import Text.XHtmlCombinators.Internal

lorem' :: XHtml4 c => Attrs -> XHtmlT (State StdGen) c
lorem' attrs = p' attrs (text =<< randomPara)

lorem :: XHtml4 c => XHtmlT (State StdGen) c
lorem = lorem' []

loremIO' :: XHtml4 c => Attrs -> XHtmlT IO c
loremIO' attrs = p' attrs (text =<< randomParaIO)

loremIO :: XHtml4 c => XHtmlT IO c
loremIO = loremIO' []

randomPara :: Monoid a => WriterT a (State StdGen) Text
randomPara = lift . fmap para $ state (randomR (1, 5))

randomParaIO :: Monoid a => WriterT a IO Text
randomParaIO = do
    gen <- lift newStdGen
    return .  para . fst $ randomR (1, 5) gen

para :: Int -> Text

para 1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas in nunc\
\ sed nunc scelerisque molestie. Curabitur in mattis sem. Curabitur quis fringilla\
\ tortor. Sed rutrum vehicula diam eu ullamcorper. Vivamus ligula justo, faucibus \
\eget pretium nec, rutrum mollis quam. Nulla hendrerit, orci eu fringilla tempor, \
\risus ligula posuere metus, vitae posuere arcu est non odio. Sed sem ipsum, porta\
\ nec venenatis eget, interdum a quam. Donec et felis vitae urna dignissim digniss\
\im in at orci. Sed varius consequat dapibus. Integer tellus eros, ullamcorper et \
\ullamcorper non, consequat in ante. In ut sem ipsum. Vivamus ante orci, cursus vi\
\tae blandit dapibus, gravida vel enim. Integer volutpat ligula in lorem auctor eg\
\et ornare massa gravida. Nullam in metus diam, eget pellentesque sem. Vestibulum \
\adipiscing accumsan nibh sit amet lobortis. Vivamus a quam turpis. Proin eu sapie\
\n sapien, ut elementum ante. Sed magna eros, consectetur eget consequat nec, pell\
\entesque vitae velit. Etiam mi tortor, cursus vel tristique non, dapibus vel mi. \
\Aenean a lorem non massa porta vulputate ut nec turpis."

para 2 = "Morbi a magna nec orci cursus semper in in lacus. Ut semper tempus lacus,\
\ ut hendrerit mauris volutpat congue. In lacus turpis, tristique eu mattis ut, ti\
\ncidunt eget sem. Ut lacinia lorem at lorem mollis tristique. In nec orci aliquam\
\ enim sagittis sagittis non vitae ante. Morbi faucibus dolor ac mi lacinia tristi\
\que. Aliquam et tortor urna, vel eleifend felis. Curabitur tellus augue, interdum\
\ ac hendrerit at, volutpat tincidunt velit. In est lorem, imperdiet eget consequa\
\t sagittis, sagittis nec quam. Duis non volutpat augue."

para 3 = "Integer semper, elit eget viverra dignissim, lorem ipsum gravida tortor, \
\ut cursus orci ante a diam. Morbi non metus et sem pellentesque dapibus sed vel m\
\assa. Fusce commodo condimentum vulputate. Nullam porta, orci nec aliquam blandit\
\, orci urna sollicitudin libero, non pharetra ligula urna id nibh. In metus nulla\
\, facilisis ut tempor nec, elementum nec nunc. Aenean sapien sem, fringilla a bla\
\ndit vitae, imperdiet nec orci. Integer sit amet nunc ut dui porttitor malesuada \
\eu non mauris. Suspendisse eget diam sed dui auctor tempus eu et erat. In non lib\
\ero id purus facilisis tempus. Nulla nec tellus eget justo dignissim sagittis sit\
\ amet vitae lorem. Donec lobortis velit id massa pretium vitae tincidunt neque ul\
\trices. Duis id erat enim, ac aliquet magna. Maecenas tempus leo eget risus trist\
\ique congue eu ut turpis."

para 4 = "Ut ultrices ante nec neque suscipit ornare. Sed adipiscing diam eu enim s\
\celerisque quis rhoncus mauris pulvinar. Nulla dignissim eros a nisl posuere elei\
\fend. Nullam leo sapien, porttitor quis commodo vel, congue quis nisl. In molesti\
\e rutrum quam at malesuada. Etiam ac pretium lorem. Pellentesque nisl erat, viver\
\ra in tempus eget, pulvinar vel nisi. Curabitur odio nisl, viverra ac faucibus se\
\d, rhoncus et neque. Sed placerat dolor ante, pulvinar ultrices arcu. Nulla id le\
\o odio, ut dignissim nisi. Phasellus id tortor libero, eget aliquet sapien. Nulla\
\ facilisi. Duis ornare ultricies dolor, sed dapibus lorem sollicitudin non. Nulla\
\m augue lorem, consequat ut faucibus ac, tempus id ipsum. Fusce sit amet libero s\
\agittis dolor convallis volutpat. Aenean at elit libero. Phasellus eget odio eget\
\ mauris vehicula luctus id a est."

para 5 = "Sed purus orci, tincidunt non tincidunt a, mattis at justo. Vivamus quis \
\nisl id justo volutpat tristique. Donec mattis, neque posuere aliquam molestie, m\
\assa ipsum fringilla erat, et vehicula lorem libero quis nisi. In tellus nunc, el\
\ementum eget blandit a, pellentesque at risus. Quisque in nulla id nibh eleifend \
\ultricies viverra non felis. Aliquam tempus, libero at ullamcorper venenatis, ero\
\s mauris consectetur urna, sit amet facilisis diam eros at tellus. Vestibulum con\
\dimentum quam non est dapibus non faucibus mi luctus. Aenean quis massa nec diam \
\varius hendrerit. Morbi tempus ornare lectus et dapibus. Sed tortor tellus, lobor\
\tis a ullamcorper id, ornare nec sapien. Quisque varius feugiat ante, eu dictum e\
\nim tempus et."
