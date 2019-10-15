---
title: Und nun die Wahlvorhersage...
isBlogPost: True
---

### Und nun die Wahlvorhersage…

von Stephan Schiffels und Andrej Fischer

Vorhersagen von Wahlergebnissen aufgrund von Umfragen gehören zum
Standardgeschäft des politischen Betriebes. Etwa wöchentlich veröffentlichen die
großen Fernsehsender Umfragen, zum Beispiel zur sogenannten “Sonntagsfrage”:
“Wenn am nächsten Sonntag Bundestagswahl wäre, wen würden Sie wählen?”

Die Ergebnisse dieser Umfragen sind eine recht umfangreiche Menge Daten, die
zusammengenommen einen Einblick in die Stimmung im Land geben und auch in
gewissem Maße eine Vorhersage für ein Wahlergebnis geben.

Solche Vorhersagen sind jedoch nur seriös wenn sie a) sämtliche Umfragen zu Rate
ziehen die zum aktuellen Zeitpunkt verfügbar sind, und b) ein Maß über die
Unsicherheit einer solchen Vorhersage angeben. Zum Beispiel sollte eine gute
Vorhersage sämtliche Umfragen aller Institute der letzten Jahre, und nicht nur
eine einzige Umfrage vom letzten Sonntag verwenden, um eine möglichst gut
informierte Schätzung für zukünftige Verläufe der Stimmung zu erreichen. Auch
sollten Ergebnisse der Umfragen in Form von Wahrscheinlichkeiten angeben werden
(etwa “mit 99% Wahrscheinlichkeit liegt der Zweitstimmenanteil der Partei XY an
der Wahl zwischen 8 und 15%”), und nicht als Einzelwert wie von den
Umfrageinstituten herausgegeben.

*****

Wir haben ein statistisches Modell zur Analyse von Umfrageergebnissen und
Vorhersage entwickelt, welches auf den beiden Bedingungen oben beruht, und
stellen heute — etwa 6 Wochen vor der Wahl zum deutschen Bundestag am 24.9.2017
— unsere Ergebnisse vor. Wir weisen explizit darauf hin dass es sich in keiner
Weise um ein abgeschlossenes Projekt handelt, sondern um *work in progress*.

Kernelement der Methode ist die Modellierung einer — prinzipiell unbekannten —
Zweitstimmen-Verteilung, die zu jedem Zeitpunkt existiert, und die durch
Umfragen verschiedener Institute und zu verschiedenen Zeiten nur ungenau
abgebildet wird. Wir kennen die wahre Verteilung nicht genau, können aber einen
Erwartungswert zu jedem Zeitpunkt angeben. Hier ist der Verlauf dieses
Erwartungswertes seit der letzten Bundestagswahl für relevante Parteien,
zusammen mit den Umfrage-Ergebnissen:

<figure>
![](/images/wahlblick_diffusion.jpg)
<figcaption>Umfrageergebnisse verschiedener Institute seit der letzten Bundestagswahl, sowie
der Erwartungswert der Zweitstimmen-Anteile in der Bevölkerung durch unser
Modell, angezeigt durch Kurven.</figcaption>
</figure>

Man sieht, dass unser Modell eine Art Mittelwert durch die vielen Ergebnisse der
Institute bildet, allerdings in gewisser Weise gewichtet durch die Anzahl der
Befragten in jeder Umfrage. Hinzu kommt, dass der Verlauf der
Zweitstimmenanteile im Modell nicht beliebig schnell schwankt, sondern einer
gewissen “Steifigkeit” unterliegt, die wir nicht vorgeben, sondern die aus den
Daten heraus vom Modell erlernt wird.

Gut sichtbar in diesem Verlauf ist etwa der kometenhafte Anstieg der SPD nach
der Bekanntgabe von Martin Schulz als Spitzenkandidat im Januar 2017, oder auch
der Aufstieg der AfD während “Flüchtlingskrise” 2015.

Ein bekannter Effekt ist die systematische Abweichung vom mittleren
Erwartungswert bestimmter Institute bei bestimmten Parteien:

<figure>
![](/images/wahlblick_biases.jpg)
<figcaption>Systematische Abweichungen zwischen Instituts-Schätzungen von unserer mittleren
Schätzung in die die Umfragen aller Institute eingehen.</figcaption>
</figure>

Man sieht, dass z.B. Insa die CDU typischerwise um 1% niedriger einschätzt als
unser Erwartungswert, der ja sämtliche Umfragen mit in die Schätzung einfließen
lässt. Ebenso schätzt Allensbach die SPD um etwa 1% höher ein.

Wie funktioniert nun eine Vorhersage? Unser Modell arbeitet
*probabilistisch,*drückt also die Unsicherheit über die Zweitstimmenverteilung
in Form einer Wahrscheinlichkeitsverteilung aus, die sich mit der Zeit ändert.
Diese ist in der Verlaufs-Abbildung oben nicht dargestellt, sondern eben nur der
Erwartungswert, aber intern handelt es sich um eine
Wahrscheinlichkeitsverteilung. Man kann also fragen, wie sich diese
Wahrscheinlichkeitsverteilung in die Zukunft bis zur Wahl entwickelt. Hierzu
gehen wir davon aus, dass sich die Stimmung auch während der nächsten 6 Wochen
noch ändern kann, und zwar mit *der gleichen Dynamik die wir aus den Umfragen
der Vergangenheit gelernt haben.*

Wir legen für die folgende Vorhersage die gesamten Umfragedaten seit der letzten
Bundestagswahl bis heute (letzte Umfrage am 2.8.2017) zu Grunde. Zum Vergleich
zeigen wir auch die Vorhersage mit Stand vom Januar 2017:

<figure>
![](/images/wahlblick_vorhersage_parteien.jpg)
</figure>

Während mit Stand vom Januar 2017 eine Vorhersage zum September noch mit einer
sehr hohen Unsicherheit verbunden ist, lassen die zusätzlichen Umfragen seit
Beginn des Jahres, und der kleiner werdende Abstand zur Wahl unsere Vorhersage
immer sicherer werden. Bei den großen Parteien können wir zum heutigen Stand
eine Vorhersage am Wahltag zu etwa 8% eingrenzen, während die Unsicherheit zu
Beginn des Jahres etwa doppelt so groß war.

Die hier angegebenen Wahrscheinlichkeiten zum Wahltag lassen uns auch andere
Fragen untersuchen. Zum Beispiel können wir fragen, mit welcher
Wahrscheinlichkeit eine Partei die Fünfprozenthürde erreicht:

<figure>
![](/images/wahlblick_fünfprozenthürde.jpg)
</figure>

Zum heutigen Zeitpunkt scheint es also so gut wie sicher dass alle hier
untersuchten Parteien die Fünfprozenthürde erreichen werden. Im Januar sah das
zumindest für die FDP noch nicht ganz so sicher aus.

Schließlich können wir unsere Vorhersage auch verwenden um auszurechnen, mit
welcher Wahrscheinlichkeit eine bestimmte Koalition nach der Wahl im Bundestag
die absolute Mehrheit erhält, wieder als Vergleich zwischen dem Stand vom Januar
2017 und dem Stand heute:

<figure>
![](/images/wahlblick_vorhersage_koalitionen.jpg)
</figure>

Diese Berechnung der Koalitionsmehrheit bezieht auch explizit die Möglichkeit
mit ein, dass kleine Parteien unter die Fünfprozenthürde fallen, was zu teils
großen Schwankungen führen kann. Das ist auch der Grund, warum Schwarz-Gelb im
Januar mit nur 2% Wahrscheinlichkeit die bewertet wurde, heute hingegen mit
immerhin 23%, da es im Januar möglich erschien dass die FDP den Wiedereinzug in
den Bundestag verfehlt. Insgesamt ergibt sich zum heutigen Zeitpunkt ein recht
klares Bild einer erneuten CDU-geführten Regierung, da sämtliche
Koalitionsoptionen ohne die CDU verschwindende Wahrscheinlichkeit für das
Erreichen der absoluten Mehrheit haben.

Es sei nochmal betont, dass diese Vorhersagen auf der Annahme beruhen, dass bis
zur Wahl keine “Wunder” eintreten, oder etwa Ereignisse die mit dramatischem
Vertrauensverlust in eine Partei oder eine Spitzenkandidatin/einen
Spitzenkandidaten einhergehen. Die Vorhersagen gehen explizit davon aus, dass
sich die Stimmung in der Bevölkerung nur so schnell verändert wie sie das
*durchschnittlich *auch in den vergangenen Jahren getan hat.

*****

Wir werden sehen, ob die hiergemachten Vorhersagen eintreten oder nicht. In
jedem Fall kann das Modell an verschiedenen Stellen noch verbessert werden, und
weitere Umfragen in den nächsten Wochen lassen uns unsere Vorhersagen weiter
einengen. *Stay tuned *für weitere Updates!
