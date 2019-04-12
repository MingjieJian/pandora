      subroutine LAKANE
     $(LU,KODE,LAB,N,NS,NL,MN1,Z,JQ,JX,GVL)
C
C     Rudolf Loeser, 1990 Apr 19
C---- Plots total diffusion terms.
C     !DASH
      save
C     !DASH
      real*8 GMAX, GMIN, GVL, Z
      integer J, JQ, JX, KODE, LU, MN1, N, NL, NS
      logical OK
      character LAB*(*)
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external PASA, BORLEY, NORI, PHELA, HI, BYE
C
C               Z(N), GVL(N,NL)
      dimension Z(*), GVL(N,*)
C
      call HI ('LAKANE')
C     !BEG
      if(NL.gt.NS) then
C
C----   Get function limits
        call PASA     (GVL, N, NS, NL, JQ, JX, GMIN, GMAX)
C----   Initialize plot image
        call BORLEY   (IMAGE, GMIN, GMAX, Z, JQ, JX, KODE, OK)
        if(OK) then
C----     Enter data
          do 100 J = NL,NS,-1
            call NORI (IMAGE, JQ, JX, Z, GVL(1,J), ALPHS(J), KODE)
  100     continue
C----     Print plot
          call PHELA  (LU, IMAGE, NS, NL, LAB)
        end if
C
      end if
C     !END
      call BYE ('LAKANE')
C
      return
      end
