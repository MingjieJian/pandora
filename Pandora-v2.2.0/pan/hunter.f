      subroutine HUNTER
     $(NO,XLMXX,XLMDR,LLY)
C
C     Rudolf Loeser, 2002 Sep 17
C---- Prints DR-calculation data, for ARNOLD.
C     (This is version 2 of HUNTER.)
C     !DASH
      save
C     !DASH
      real*8 ONE, XLMD2, XLMD3, XLMDR, XLMXC, XLMXP, XLMXX
      integer I, LLY, NO
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(162),XLMXC)
      equivalence (RZQ(163),XLMXP)
      equivalence (RZQ(164),XLMD2)
      equivalence (RZQ(154),XLMD3)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
C               XLMXX(LLY), XLMDR(LLY)
      dimension XLMXX(*),   XLMDR(*)
C
      call HI ('HUNTER')
C     !BEG
      write (NO,100)
  100 format(' ','The total Lyman alpha line opacity, OPAC, is ',
     $           'defined as the sum of an absorption component and a ',
     $           'scattering component. The'/
     $       ' ','absorption fraction decreases from 1 in the core to ',
     $           '0 in the far wings. The absorption fraction as a ',
     $           'function of distance'/
     $       ' ','from line center in Doppler widths, x, is called ',
     $           'DR(x). It is either computed from a formula ',
     $           'controlled by input'/
     $       ' ','parameters, or interpolated from an input table. ',
     $           'The absorption component is then DR times OPAC, ',
     $           'and the scattering'/
     $       ' ','component is (1-DR) times OPAC [see "LyAabs" (= # ',
     $           '11), "LyBabs" (= # 34), "LyAsct" (= # 16), and ',
     $           '"LyBsct" (= # 36)'/
     $       ' ','in printout section BACKGROUND, above].')
      call LINER   (1, NO)
      if(XLMXC.eq.(-ONE)) then
        write (NO,101)
  101   format(' ','This run uses the following table:'//
     $         ' ',10X,'LMXX (= x)',4X,'LMDR (= DR)')
        call LINER (1, NO)
        write (NO,102) (I,XLMXX(I),XLMDR(I),I=1,LLY)
  102   format(5(' ',I5,1P2E15.5/))
      else
        write (NO,103) XLMXC,XLMXP,XLMD2,XLMD3
  103   format(' ','This run uses the formula with parameters:'//
     $         ' ','LMXC (= xc) =',1PE14.6,5X,'LMXP (= xp) =',E14.6/
     $         ' ','(The lower limit of DR, in the wings, is LMDL2 =',
     $             E14.6,' for line 2/1 and is LMDL3 =',E14.6,
     $             ' for line 3/1;'/
     $         ' ','for higher lines it is computed as needed.)')
      end if
C     !END
      call BYE ('HUNTER')
C
      return
      end
