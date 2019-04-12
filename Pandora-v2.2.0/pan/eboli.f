      subroutine EBOLI
     $(XNUC,XNU,LRQ,NPQ,HYDR,MESS,GOOD)
C
C     Rudolf Loeser, 1992 Jan 03
C---- Computes the mean radius, for DOMINO.
C     (This is version 2 of EBOLI.)
C     !DASH
      save
C     !DASH
      real*8 ELL, ENN, HALF, ONE, THREE, TWO, XNU, XNUC, ZERO
      integer LRQ, LUEO, NPQ
      logical GOOD, HYDR, MESS
C     !COM
C---- OLIVIA      as of 2006 Mar 08
      real*8      XNUKH,RBAR,DNU,PT,R,TERJ
      integer     IUCE,ILCE
      common      /OLIVIA/ XNUKH,RBAR,DNU,PT,R
      common      /OLIVIB/ TERJ
      common      /OLIVIC/ IUCE,ILCE
C     Parameters for ERIKA: calculation of Collisional Ionization
C     Integral, for the impact-parameter method.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
C     !DASH
C     !EJECT
      external MESHED, PHAROT, MASHED, HI, BYE
C
      call HI ('EBOLI')
C     !BEG
      ELL = LRQ
      ENN = NPQ
      if(ELL.eq.-ONE) then
        if(HYDR) then
          ELL = ENN-ONE
        else
          ELL = (TWO+ENN)/TWO
        end if
      end if
      RBAR = HALF*(THREE*(XNUKH/(XNUC-XNU))-ELL*(ELL+ONE))
C
      GOOD = RBAR.gt.ZERO
C
      if((.not.GOOD).and.MESS) then
        call MESHED ('EBOLI/DOMINO', 3)
        call PHAROT (LUEO)
        write (LUEO,100) RBAR,XNUC,XNU,LRQ,NPQ,ELL
  100   format(' ','Unable to compute r-bar',1PE20.8//
     $         ' ','XNUC =',E16.8,5X,'XNU =',E16.8,5X,'"ell" =',I4,5X,
     $             'n =',I4,5X,'ell =',0PF4.0//
     $         ' ','>>>>>>>>>> Use Van Regemorter instead.')
        call MASHED ('EBOLI/DOMINO')
      end if
C     !END
      call BYE ('EBOLI')
C
      return
      end
