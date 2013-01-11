{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Layout (layout,groupSort) where

import Site.Map
import Data.Text (Text)
import Data.List(groupBy)
import Data.List(sortBy)
import Text.Hamlet (hamlet,HtmlUrl)
import Text.Blaze.Html.Renderer.String (renderHtml)

layout f subtitle = [hamlet|
!!!
<head>
    ^{headblock}
<body>
    <a href="https://github.com/lucasdicioccio/haskell-paris-src"><img style="position: absolute; top: 0; right: 0; border: 0;" src="https://s3.amazonaws.com/github/ribbons/forkme_right_gray_6d6d6d.png" alt="Fork me on GitHub"></a>
    <div class="container">
        <div class="row-fluid span12">
            ^{header subtitle}
            ^{f}
            ^{footer}
|]

headblock :: HtmlUrl Page
headblock = [hamlet|
<meta charset="utf-8">
<title>
    Haskell-Paris
<style>
    .nospam{display:none} 
<link rel="shortcut icon" type="image/x-icon" href="favicon.ico">
<link rel="stylesheet" type="text/css" href="/css/metro-bootstrap.css">
<meta name="author" content="Lucas DiCioccio">
<meta name="description" content="archive des talks">
|]

header :: Text -> HtmlUrl Page
header subtitle = [hamlet|
<header class="jumbotron masthead">
    <div class="inner">
        <h1>
            <a href="/" alt="haskell-paris">
                <img class="logo" src="/img/haskell-paris.jpg" alt="logo" title="Haskell Paris"> HASKELL-PARIS
        <h2> #{subtitle}
|]

footer :: HtmlUrl Page
footer = [hamlet|
<footer class="footer">
    <p>
        Ce site a été réalisé à la base du thème metro de <a href="http://twitter.com/talkslab" target="_blank">talkslab</a>, produit par <a href="http://twitter.com/gsferreira" target="_blank">gsferreira</a>, <a href="http://twitter.com/nelsonreis" target="_blank">nelsonreis</a> and <a href="http://twitter.com/ruimlneves" target="_blank">ruimlneves</a>.
|]

groupSort :: (Eq b,Eq c,Ord b,Ord c) => (a -> b) -> (a -> c) -> [a] -> [[a]]
groupSort f g xs = map (sortBy g') $ groupBy f' $ (sortBy h) xs
    where f' m1 m2 = f m1 == f m2
          h  m1 m2 = f m1 `compare` f m2
          g' m1 m2 = g m2 `compare` g m1 -- we want decreasing

