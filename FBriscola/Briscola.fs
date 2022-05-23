//Questo codice è stato creato da Giulio Sorrentino basandosi
//sul suo progetto della briscola a console mai pubblicato ed
//è quindi disponibile sotto licenza GPL
    module Briscola
    type public ElaboratoreCarte = { mutable NumeroCarte: uint16; mutable Doppione: bool array; mutable CartaBriscola: uint16; mutable inizio: bool; mutable BriscolaDapunti: bool}
    let inline getNumeroCarte(e: ElaboratoreCarte): uint16 = e.NumeroCarte
    let inline getCartaBriscola(e: ElaboratoreCarte): uint16 = e.CartaBriscola
    let inline getBriscolaDaPunti(e: ElaboratoreCarte): bool = e.BriscolaDapunti
    let public getCartaInt(elaboratoreCarte: ElaboratoreCarte) uint16 =
        let mutable random = System.Random();       
        let mutable fine: uint16= (uint16) (random.Next(0,40))
        let mutable carta: uint16=(fine+1us)%elaboratoreCarte.NumeroCarte
        let mutable valore: uint16= 1us
        while elaboratoreCarte.Doppione[(int) (carta)] && not ( carta=fine ) do
            carta <- (carta+1us) % elaboratoreCarte.NumeroCarte
            if elaboratoreCarte.Doppione[(int) (carta)] then
             carta <-elaboratoreCarte.NumeroCarte+1us
            elif elaboratoreCarte.inizio then 
                valore <- carta%10us
                if not( elaboratoreCarte.BriscolaDapunti ) && ( valore=0us || valore=2us || valore>6us) then
                    carta <- carta+1us
                elaboratoreCarte.CartaBriscola <- carta
                elaboratoreCarte.inizio <- false
            else
                elaboratoreCarte.Doppione[(int) carta] <- true
        carta

    type public Carta = {mutable Seme: uint16; mutable Valore: uint16; mutable Punteggio: uint16; mutable SemeStr: string }

    let inline getSeme(carta: Carta) uint16 = carta.Seme
    let inline getValore(carta: Carta) uint16 = carta.Valore
    let inline getPunteggio(carta: Carta) uint16 = carta.Punteggio
    let inline getSemeStr(carta: Carta) string = carta.SemeStr
    let inline stessoSeme(c: Carta, c1: Carta) bool = c.Seme = c1.Seme
    let inline getSeme(c: uint16): uint16 = c/10us
    let inline getValore(c: uint16): uint16 = c%10us
    let getPunteggio(c: uint16) uint16 =
        let valore: uint16=getValore c
        if valore=0us then
            11us
        elif valore=2us then
            10us
        elif valore=9us then
            4us
        elif valore=8us then
            3us
        elif valore=7us then 
            2us
        else
            0us
    let getSemeStr(c: uint16) string =
        let seme: uint16 = getSeme(c)
        if seme=0us then
            "Bastoni"
        elif seme=1us then
            "Coppe"
        elif seme=2us then
            "Denari"
        else
            "Spade"

    type public Mazzo = {mutable carte: Carta array; numeroCarte: uint16; elaboratoreCarte: ElaboratoreCarte}
    let inline getNumeroCarte(m: Mazzo) int = m.carte.Length
    let inline getCarta(m: Mazzo, quale: uint16) Carta = m.carte[(int) (quale)]
    let mischiaMazzo(m: Mazzo) = 
        let mutable i: uint16=1us
        let mutable carta: uint16=0us
        let mutable seme: string=""
        let mutable punteggio: uint16=0us
        if m.carte=Array.empty then
            m.carte <- Array.zeroCreate 40
            carta <- getCartaInt(m.elaboratoreCarte)
            punteggio <- getPunteggio(carta)
            seme <- getSemeStr(carta)
            m.carte[0] <- {Seme=getSeme(carta); Valore=getValore(carta); Punteggio = punteggio ; SemeStr=seme}
            while not (carta=getCartaBriscola(m.elaboratoreCarte)) do
                m.carte[(int) (i)] <- {Seme=getSeme(carta); Valore=getValore(carta); Punteggio = punteggio ; SemeStr=seme}
                
            

   
