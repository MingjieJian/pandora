      subroutine TELLUM
     $(XLM,ITAU,IM,IP,JM,JP,NL,EN,WN,AN,SA,ST,XX,AA,PN,TN,SGM,SGP,G)
C
C     Rudolf Loeser, 2003 Jan 10
C---- Prints details, for MULLET.
C     !DASH
      save
C     !DASH
      real*8 AA, AN, EN, G, PN, SA, SGM, SGP, ST, TN, WN, XLM, XX
      integer I, IM, IP, ITAU, JM, JP, LIM, LUEO, N, NL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               EN(NL), WN(NL), TN(NL), AN(NL), XX(NL), AA(NL), PN(NL),
      dimension EN(*),  WN(*),  TN(*),  AN(*),  XX(*),  AA(*),  PN(*),
C
C               SA(NL), ST(NL)
     $          SA(*),  ST(*)
C     !EJECT
C
      call HI ('TELLUM')
C     !BEG
      call LINER    (1, LUEO)
      write (LUEO,100) G,XLM,ITAU,JM,IM,IP,JP
  100 format(' ','g =',1PE16.8,' for LM =',E20.12,' and i =',I5,10X,
     $           '(',4I5,'  )'//
     $       ' ',2X,'N',19X,'LN',8X,'A(N,1)',4X,'sum A(N,k)',
     $           9X,'STKFN',8X,'a(i,N)',8X,'x(i,N)',6X,'phi(a,x)',
     $           2X,'term for sum')
C
      if(IP.eq.0) then
        LIM = IM
      else
        LIM = JP
      end if
C
      do 103 I = JM,LIM
        N = EN(I)
        if(I.eq.IM) then
          write (LUEO,101) N,'-',WN(I),AN(I),SA(I),ST(I),AA(I),XX(I),
     $                     PN(I),TN(I)
  101     format(' ',I3,A1,1PE20.12,7E14.6)
          write (LUEO,102) XLM,SGM,SGP
  102     format(' ',1PE24.12,3X,'-sum, SGM =',E14.6,3X,'+sum, SGP =',
     $               E14.6)
        else if(I.eq.IP) then
          write (LUEO,101) N,'+',WN(I),AN(I),SA(I),ST(I),AA(I),XX(I),
     $                     PN(I),TN(I)
        else
          write (LUEO,101) N,' ',WN(I),AN(I),SA(I),ST(I),AA(I),XX(I),
     $                     PN(I),TN(I)
        end if
  103 continue
C     !END
      call BYE ('TELLUM')
C
      return
      end
