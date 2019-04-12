      subroutine QUILL
     $(NO,IIFLAG,INCDNT,N,SCAT,XKPC,Z,TAU,Y,S,BHS,B,TE,FD,CNXP)
C
C     Rudolf Loeser, 1980 Mar 13
C---- Prints tables, for SHOOT.
C     !DASH
      save
C     !DASH
      real*8 B, BHS, CNXP, FD, S, SCAT, TAU, TE, XKPC, Y, Z, ZERO
      integer FLAG, I, IIFLAG, INDX, N, NO
      logical INCDNT
      character BLANK*1, DOLLAR*1, MBHSB*1, MSBHS*1, MT*1, MYS*1,
     $          TERM*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(37),DOLLAR)
C     !DASH
      external MARKI, GIMEL, SHIM, HI, BYE
C
C               SCAT(N), XKPC(N), Z(N), TAU(N), FD(N), CNXP(N), BHS(N),
      dimension SCAT(*), XKPC(*), Z(*), TAU(*), FD(*), CNXP(*), BHS(*),
C
C               B(N), TE(N), Y(N), S(N)
     $          B(*), TE(*), Y(*), S(*)
C
      dimension MYS(2), MSBHS(2), MBHSB(2)
C     !EJECT
C
      call HI ('QUILL')
C     !BEG
      MYS(2)   = BLANK
      MSBHS(2) = BLANK
      MBHSB(2) = BLANK
      FLAG = IIFLAG/1000
      INDX = IIFLAG-1000*FLAG
C
      do 102 I = 1,N
        call MARKI (I, INDX, MT, DOLLAR, BLANK)
        call GIMEL (Y(I), S(I), BHS(I), B(I), FLAG, MYS, MSBHS, MBHSB)
C
        if(INCDNT.and.(CNXP(I).ne.ZERO)) then
          write (TERM,100) CNXP(I)
  100     format(1PE10.2)
        else
          TERM = BLANK
        end if
C
        write (NO,101) I,SCAT(I),XKPC(I),Z(I),TAU(I),MT,Y(I),MYS(1),
     $                 S(I),MSBHS(1),BHS(I),MBHSB(1),B(I),TE(I),
     $                 FD(I),TERM
  101   format(' ',I3,1PE12.4,E12.4,E10.2,E12.4,A1,E11.4,1X,A1,E10.3,
     $             1X,A1,E10.3,1X,A1,E10.3,E10.2,E10.2,A10)
C
        call SHIM  (I, 5, NO)
  102 continue
C     !END
      call BYE ('QUILL')
C
      return
      end
