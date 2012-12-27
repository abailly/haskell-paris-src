{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Index (homepage,degradedHomepage) where

import Site.Map
import Views.Layout
import Views.Talk
import Views.Meetup
import Models.Meetup
import Text.Hamlet (hamlet,HtmlUrl)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.List(intercalate)
import Data.List(groupBy)
import Data.List(sortBy)

homepage meetups = renderHtml $ layout [hamlet|
<div class="row span12">
    <div class="well span6">
        ^{presentationText}
    <div class="well span5">
        <h2> Proposer un talk
        ^{newTalkForm}
<h2>Past Meetups
^{formatMeetups meetups}
|] "Site dédié au groupe Haskell Paris" render


degradedHomepage meetups = renderHtml $ layout [hamlet|
<div class="row span12">
    ^{presentationText}
<div class="row span12">
    <h2>Base de données plantée
    <p>Malheureusement le serveur ne peut établir de connexion à notre base de
        données. Du coup on ne peut pas vous présenter la liste des meetups passés.
^{formatMeetups meetups}
|] "Site dédié au groupe Haskell Paris" render

presentationText = [hamlet|
        <h2> Next meetup
        <p>Nous essayons de nous rencontrer le <strong>second Lundi du mois</strong>.
        <p>Pour se tenir informé et s'enregistrer pour le prochain meetup, merci de s'inscrire sur le <a href="http://meetup.haskell-paris.fr">compte meetup de haskell-paris</a>.
        <p>Si vous voulez présenter votre entreprise, par exemple pour embaucher ou pour présenter un produit, merci de sponsoriser un meetup en offrant un lieu d'accueil ou de la nourriture. Pour ce faire, contactez lucas<span class="nospam">nospam</span>@<span class="nospam">nospam</span>dicioccio.fr .
|]

groupSort :: (Eq b,Eq c,Ord c) => (a -> b) -> (a -> c) -> [a] -> [[a]]
groupSort f g xs = groupBy f' $ sortBy g' xs
    where f' m1 m2 = f m1 == f m2
          g' m1 m2 = g m2 `compare` g m1 -- we want decreasing

yearMonth m = (year m, month m)

formatMeetups meetups = [hamlet|
$forall byYear <- groupSort year yearMonth meetups
    <h3>#{year $ head byYear}
    $forall m <- byYear
        ^{formatMeetup m}
|]
