{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Views.Meetup (listMeetupPage,formatMeetup,editMeetupPage) where

import Site.Map
import Views.Layout
import Models.Meetup
import Text.Hamlet (hamlet,HtmlUrl)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.List(intercalate)

formatMeetup m = [hamlet|
    <h4>#{month m} (#{place m})
    <div class="summary">
    <p>Sponsors: #{intercalate " | " $ sponsors m}
    $forall l <- (lines $ summary m)
        <p>#{l}
    <p>Slides: 
        <ul>
            $forall l <- (slides m)
                <li><a href="#{l}">#{l}</a>
    <p>Liens: 
        <ul>
            $forall l <- (links m)
                <li><a href="#{l}">#{l}</a>
|]

listMeetupPage meetups = renderHtml $ layout [hamlet|
<div class="row span12">
    <div class="row span7">
        $forall (m,oId) <- meetups
            ^{formatMeetup m}
            <a href="/meetup/#{show oId}">Edit
    <div class="row span5">
        <h3>Nouveau meetup
        ^{newMeetupForm}
|] "Liste des meetups" render

editMeetupPage meetups = renderHtml $ layout [hamlet|
<div class="row span12">
    $forall (m,oId) <- meetups
        <div class="row span5">
            ^{formatMeetup m}
        <div class="row span5">
            ^{updateMeetupForm (show oId) m}
|] "Editer un meetup" render

newMeetupForm = [hamlet|
        <form accept-charset="UTF-8" action="/meetup" class="form" id="new_meetup" method="post">
            <div>
                <input name="utf8" type="hidden" value="&#x2713;">
            <div>
                <label class="string required" for="meetup_year">
                    <abbr title="requis">*
                    Année
                <input class="string required" id="meetup_year" maxlength="4" name="meetup[year]" placeholder="2013" required="required" size="4" type="text">
            <div>
                <label class="string required" for="meetup_month">
                    <abbr title="requis">*
                    Month
                <input class="string required" id="meetup_month" maxlength="255" name="meetup[month]" placeholder="Janvier" required="required" size="20" type="text">
            <div>
                <label class="string required" for="meetup_place">
                    <abbr title="requis">*
                    Lieu
                <input class="string required" id="meetup_place" maxlength="255" name="meetup[place]" placeholder="Planète Terre" required="required" size="140" type="text">
            <div>
                <label class="string required" for="meetup_summary">
                    <abbr title="requis">*
                    Résumé
                <textarea id="meetup_summary" name="meetup[summary]" class="text required" placeholder="lorem ipsum…" required="required">
            <div>
                <label class="string required" for="meetup_sponsors">
                    <abbr title="requis">*
                    Sponsors (un par ligne)
                <textarea id="meetup_sponsors" name="meetup[sponsors]" class="text required" placeholder="lorem ipsum…" required="required">
            <div>
                <label class="string" for="meetup_links">
                    <abbr title="requis">(opt)
                    Links (un par ligne)
                <textarea id="meetup_links" name="meetup[links]" class="text" placeholder="lorem ipsum…">
            <div>
                <label class="string" for="meetup_slides">
                    <abbr title="optionnel">(opt)
                    Slides (un par ligne)
                <textarea id="meetup_slides" name="meetup[slides]" class="text" placeholder="lorem ipsum…">
            <div>
                <input class="button btn" name="commit" type="submit" value="Créer">
|]

updateMeetupForm oId meetup = [hamlet|
        <form accept-charset="UTF-8" action="/meetup/#{oId}" class="form" id="new_meetup" method="post">
            <div>
                <input name="utf8" type="hidden" value="&#x2713;">
            <div>
                <label class="string required" for="meetup_year">
                    <abbr title="requis">*
                    Année
                <input class="string required" id="meetup_year" maxlength="4" name="meetup[year]" placeholder="2013" required="required" size="4" type="text" value="#{year meetup}">
            <div>
                <label class="string required" for="meetup_month">
                    <abbr title="requis">*
                    Month
                <input class="string required" id="meetup_month" maxlength="255" name="meetup[month]" placeholder="Janvier" required="required" size="20" type="text" value="#{month meetup}">
            <div>
                <label class="string required" for="meetup_place">
                    <abbr title="requis">*
                    Lieu
                <input class="string required" id="meetup_place" maxlength="255" name="meetup[place]" placeholder="Planète Terre" required="required" size="140" type="text" value="#{place meetup}">
            <div>
                <label class="string required" for="meetup_summary">
                    <abbr title="requis">*
                    Résumé
                <textarea id="meetup_summary" name="meetup[summary]" class="text required" placeholder="lorem ipsum…" required="required">
                    #{summary meetup}
            <div>
                <label class="string required" for="meetup_sponsors">
                    <abbr title="requis">*
                    Sponsors (un par ligne)
                <textarea id="meetup_sponsors" name="meetup[sponsors]" class="text required" placeholder="lorem ipsum…" required="required">
                    #{unlines $ sponsors meetup}
            <div>
                <label class="string" for="meetup_links">
                    <abbr title="requis">(opt)
                    Links (un par ligne)
                <textarea id="meetup_links" name="meetup[links]" class="text" placeholder="lorem ipsum…">
                    #{unlines $ links meetup}
            <div>
                <label class="string" for="meetup_slides">
                    <abbr title="optionnel">(opt)
                    Slides (un par ligne)
                <textarea id="meetup_slides" name="meetup[slides]" class="text" placeholder="lorem ipsum…">
                    #{unlines $ slides meetup}
            <div>
                <input class="button btn" name="commit" type="submit" value="Créer">
|]
