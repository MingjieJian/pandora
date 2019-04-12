      subroutine MULLET
     $(XLM,EN,WN,DN,AN,SA,SF,PE,XX,AA,PN,ST,TN,NL,XL,H1,ITAU,G,DMPI)
C
C     Rudolf Loeser, 2003 Jan 08
C---- Computes the sum g(i,LM) for ROWAN.
C     !DASH
      save
C     !DASH
      real*8 AA, AN, DN, EN, G, H1, PE, PN, SA, SF, SGM, SGP, ST, TN,
     $       WN, XL, XLM, XX, ZERO
      integer I, IM, IP, ITAU, JM, JP, N, NL, NM
      logical DMPI, STOP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external TUMULT, LUMMET, TELLUM, HI, BYE
C
C               WN(NL), DN(NL), TN(NL), EN(NL), AN(NL), AA(NL), XX(NL),
      dimension WN(*),  DN(*),  TN(*),  EN(*),  AN(*),  AA(*),  XX(*),
C
C               PN(NL), SA(NL), SF(NL), ST(NL)
     $          PN(*),  SA(*),  SF(*),  ST(*)
C
      call HI ('MULLET')
C     !BEG
      NM = NL-15
      call TUMULT     (WN, NL, NM, XLM, IM, IP)
C
      SGM = ZERO
      do 100 I = IM,1,-1
        N = EN(I)
        ST(I) = PE*SF(I)
        call LUMMET   (XLM, XL, H1, N, WN(I), DN(I), AN(I), SA(I),
     $                 ST(I), XX(I), AA(I), PN(I), TN(I), SGM, STOP)
        JM = I
        if(STOP) then
          goto 101
        end if
  100 continue
  101 continue
C
      SGP = ZERO
      if(IP.ne.0) then
        do 102 I = IP,NM,+1
          N = EN(I)
          ST(I) = PE*SF(I)
          call LUMMET (XLM, XL, H1, N, WN(I), DN(I), AN(I), SA(I),
     $                 ST(I), XX(I), AA(I), PN(I), TN(I), SGP, STOP)
          JP = I
          if(STOP) then
            goto 103
          end if
  102   continue
  103   continue
      else
        JP = 0
      end if
C
      G = SGM+SGP
C
      if(DMPI) then
        call TELLUM   (XLM, ITAU, IM, IP, JM, JP, NL, EN, WN, AN, SA,
     $                 ST, XX, AA, PN, TN, SGM, SGP, G)
      end if
C     !END
      call BYE ('MULLET')
C
      return
      end
