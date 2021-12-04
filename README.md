# DarkTower
Ein kleines Spiel für einen Wettbewerb an der Uni-Bielefeld. 
Es hat den zweiten Platz gewonnen! :)

Hier kann man es spielen, ohne es selber kompilieren zu müssen:
https://replit.com/@fabianpichl/Projekt

## Damalige Readme:

Danke fuers Lesen der Readme!
Am Ende der Readme gibt es ein Walkthrough, falls der Weg durchs Spiel irgendwo
unklar sein sollte!
  

Bitte beachte Folgendes beim Spielen:

- Das Konsolenfenster sollte 80x24 gross sein. Wenn es kleiner wird, sieht die 
  Formatierung wahrscheinich doof aus!
  
- Das Spiel laesst sich durch Aufruf der "main"-Funktion starten

- Du schaust generell immer nach Norden. Vier Ansichten pro Location waren mir
  zu viel :D

- Es gibt folgende Befehle:
  - nach norden/sueden/westen/osten -> Du bewegst dich (wenn moeglich) in die
    entsprechende Richtung
  - umsehen -> Du siehst dich um und bemerkst alle Dinge oder Personen, mit
    denen du interagieren kannst
  - wo bin ich -> Du fragst dich laut wo du bist und eine Stimme aus dem Off
    antwortet dir
  - rede mit X -> Du sprichst die Person/ das Objekt mit der Bezeichnung X an
    -> Manchmal geben Charaktere nur einen Kommentar ab, manchmal beginnst du 
       einen Dialog
       -> "tschau" beendet (meistens) einen Dialog
  - inventar -> Listet dein Inventar auf
  - untersuche X -> Du siehst dir einen Gegenstand, eine Person oder ein Item
    (aus deinem Inventar) genauer an
  - benutze X -> Du interagierst mit Gegenstand X (geht auch mit Items aus dem
    Inventar!)
  - save X -> Speichert das Spiel in Spielstand X.
    -> X muss eine Zahl sein.
    -> Es wird ein Ordner "saves" in dem Verzeichnis angelegt, in dem die .hs
       Dateien liegen. Ein Fehler beim Speichern beendet NICHT das Spiel.
  - load X -> Laedt Spielstand X.
    -> X muss eine Zahl sein.
    -> Es wird aus dem "saves"-Ordner geladen. Ein Fehler beim Laden beendet
       NICHT das Spiel.
    
- Umlaute muessen als ae/oe/ue geschrieben werden!

- In der gameLoop-Methode ist der Befehl fuers Teleportieren auskommentiert.
  Wenn du moechtest, kannst du ihn einkommentieren und dich dann im Spiel mit
  z.B. "tp dorf" herumteleportieren. Eine vollstaendige Liste mit den
  Bezeichnungen fuer die einzelnen Orte sind direkt unter "-- locations -----" 
  in "tpLocations" zu finden. 


Hinweise fuers Durchsehen des Codes:

Wenn man so lange Zeit hat sich an sein eigenes Projekt zu gewoehnen, verliert
man schnell den Sinn dafuer, wie uebersichtlich das Ganze fuer andere ist.
Deshalb habe ich hier einmal kurz die Dinge zusammengefasst, die vielleicht
etwas unuebersichtlich geschrieben sind:

gameLoop
Die Methode gameLoop gibt immer zuerst einen Trennstrich aus, um den Ueberblick
zu behalten, welches Bild und welcher Text gerade aktuell sind. Danach wird das
Bild der aktuellen Location ausgegeben und ueberprueft, ob spezieller Text aus-
gegeben werden soll (ist das Argument sTxt nicht leer?). Wenn nein, dann wird
die Beschreibung der aktuellen Location ausgegeben.
Danach wird ein input eingelesen und sofort in lowercase umgewandelt, um das
Patternmatching zu vereinfachen, bei dem auf die verschiedenen Kommandos abge-
fragt wird. "nach", "rede mit", "benutze" und "untersuche" funktionieren dabei
nach dem gleichen Prinzip: Ueberpruefe, ob in der aktuellen Location eine Aktion
hinterlegt ist, die auf die eingabe passt und mit "N*" "R*", "B*, "U*" oder "I*"
beginnt. Aktionen mit "I*" stehen dabei fuer Ortsgebundene Aktionen mit einem 
Gegenstand aus dem Inventar.
"save" wandelt die Eingabe zu einem Int um und versucht, einen Spielstand anzu-
legen. Klappt dies nicht, wird ein Fehler ausgegeben, ohne das Spiel zu beenden.
"load" funktioniert aehnlich.
"tp" nutzt das dictionary "tpLocations" um die Eingabe auszuwerten und den 
Spieler an einen anderen Ort zu teleportieren.
Sollte kein Befehl erkannt werden, wird die Eingabe direkt in der Location
gesucht. Das wird vor allem in Dialogen zur Gespraechsthemenauswahl genutzt.

Location
Eine Location hat eine ID, einen Namen (der ausgegeben wird wenn der Befehl
"wo bin ich" eingegeben wird) ein Ascii-Bild, eine Beschreibung und
Interaktionen (*_Int). Die Interaktionen bestehen aus einem Dictionary, dass
Ints als keys und Methoden als values beinhaltet. Die Methoden rufen meist nur
gameLoop auf und uebergeben einen veraenderten gmst.
Dialoge werden als Locations gespeichert.

Save
- Wenn noetig wird ein Ordner "saves" angelegt.
- Die nummerierte Datei wird mit dem Inhalt beschrieben
  - In die erste Zeile kommt der Spielername
  - In die zweite Zeile kommt die aktuelle Location, als ID gespeichert
  - In die dritte Zeile kommt der Inventarinhalt
  - In die vierte Zeile kommen gespeicherte Events
- ein neuer gameLoop wird aufgerufen

Load
- Der Dateiname wird in handle gespeichert
- content' wird der Inhalt der Datei zugewiesen
- content ist eine als Liste verpackte Version von content'
- name, Location, inventar und events werden content entnommen
- ein neuer gameLoop wird aufgerufen



---

## Walkthrough:

 1.) Optional: Rede mit dem Arbeiter am Dorfeingang
 
 2.) Optional: Rede mit dem Dorfvorsteher noerdlich vom Dorfeingang
 
 3.) Gehe vom Dorfeingang aus immer weiter nach Osten, bis der Orden aus-
     geschildert ist. Gehe dann nach Sueden und benutze das Burgtor. Gehe nach 
     Norden und rede mit Gottfried. Hol dir von ihm die Erlaubnis beim Weg-
     weiser nach Osten gehen zu duerfen.
     
 4.) Kehre ins Dorf zurueck. Oestlich des Brunnens bzw. noerdlich des Platzes
     steht ein Soldat Wache. Ueberrede ihn, dir eine Gurke zu geben.
     
 5.) Gehe zum Wegweiser zurueck. Gehe von hier nach Osten und benutze die Gurke.
     Jetzt hast du Zugang zur Hoehle.
     
 6.) Optional: Rede in der Hoehle mehrmals mit der Wache. Sie gibt dir Tipps,
     wie das Geschriebene zu entschluesseln ist.
     
 7.) Benutze die Hebel und gebe dann das Passwort ein: 9630528417
 
 8.) Nimm den Bogen und kehre zu Gottfried zurueck.
     --> Wenn Gottfried bereits zu den Perlinseln aufgebrochen ist, ueberspringe
         diesen Schritt
 
 9.) Gehe zum Strand und rede mit der Wache. Lass dich zu den Perlinseln bringen
 
10.) Gehe auf den Inseln zwei mal nach Sueden

11.) Rede mit gottfried bzw. siegfriede, wenn gottfried verschwunden ist

12.) Frage nach dem Plan.

13.) Kehre zum Festland zurueck. Gehe vom Strand aus rechts.

14.) Gehe nach Osten und ueberrede die Anglerin, dir eine Angel zu geben

15.) Kehre zur Insel zurueck. Benutze die Angel zwischen Hafen und Camp um einen
     Fisch zu angeln
     
16.) Tausche im jetzt verlassenen Camp den Fisch gegen die Muenze ein.

17.) Kehre zum Brunnen in der Stadt zurueck und benutze die Muenze.

18.) Gehe zum Pfad westlich der Stadt und benutze den roten Armreif.

19.) Betritt das Grab und gehe nach Norden

20.) Benutze die Kugeln/Orbs wie folgt:
     - Die Rote im mittleren Raumteil (findest du im Grab selber)
     - Die Gruene im rechten Raumteil (findest du im Bergpass)
     - Die Blaue im linken Raumteil (bekommst du von der Anglerin)
     
21.) Benutze die Feuerschalen im mittleren Raumteil.

22.) Gehe ganz zum Anfang zurueck, zum schwarzen Turm.

23.) Benutze das Tor

24.) ???

