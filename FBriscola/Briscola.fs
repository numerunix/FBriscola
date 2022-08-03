//Questo codice è stato creato da Giulio Sorrentino basandosi
//sul suo progetto della briscola a console mai pubblicato ed
//è quindi disponibile sotto licenza GPL
    module Briscola
    let getCartaInt(numeroCarte: uint16; doppione: bool array; cartaBriscola: uint16; inizio: bool; briscolaDapunti: bool): uint16 =
        let mutable random = System.Random();       
        let mutable fine: uint16= (uint16) (random.Next(0,40))
        let mutable carta: uint16=(fine+1us)%numeroCarte
        let mutable valore: uint16= 1us
        while doppione[(int) (carta)] && not ( carta=fine ) do
            carta <- (carta+1us) % numeroCarte
            if doppione[(int) (carta)] then
             carta <-numeroCarte+1us
            elif inizio then 
                valore <- carta%10us
                if not( briscolaDapunti ) && ( valore=0us || valore=2us || valore>6us) then
                    carta <- carta+1us
                cartaBriscola <- carta
                inizio <- false
            else
                doppione[(int) carta] <- true

    let getPunteggio(c: uint16) uint16 =
        let valore: uint16=c%10us
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
        let seme: uint16 =c/10us
        if seme=0us then
            "Bastoni"
        elif seme=1us then
            "Coppe"
        elif seme=2us then
            "Denari"
        else
            "Spade"

    let mischiaMazzo(carte: uint16 array; numeroCarte: uint16; NumeroCarte: uint16; Doppione: bool array; CartaBriscola: uint16; inizio: bool; BriscolaDapunti: bool) = 
        let mutable i: uint16=1us
        let mutable carta: uint16=0us
        let mutable seme: string=""
        let mutable punteggio: uint16=0us
        if carte=Array.empty then
            carte <- Array.zeroCreate 40
            carta <- getCartaInt(NumeroCarte: uint16; Doppione: bool array; CartaBriscola: uint16; inizio: bool; BriscolaDapunti: bool)
            punteggio <- carta%10us
            seme <- carta/10us
            m.carte[0] <- {carta/10us; Valore=carta%10us; Punteggio = punteggio ; SemeStr=seme}
            while not (carta=CartaBriscola) do
                m.carte[(int) (i)] <- {Seme=getSeme(carta); Valore=getValore(carta); Punteggio = punteggio ; SemeStr=seme}
                
            

   
