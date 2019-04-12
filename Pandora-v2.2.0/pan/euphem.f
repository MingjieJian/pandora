      subroutine EUPHEM
     $(X,NTMX,TAB,NT,WLO,WHI)
C
C     Rudolf Loeser, 2004 Aug 06
C---- Enters a set of wavelengths to provide adequately for the
C     background Hydrogen Lyman lines, for TRAVIS.
C     (This is version 4 of EUPHEM.)
C     !DASH
      save
C     !DASH
      real*8 DS, TAB, WHI, WLO, X, ZERO
      integer I, K, LIM, LYM, NT, NTMX
C     !COM
C---- NOTHER      as of 2004 Jun 25
      integer     NIAUGM
      parameter   (NIAUGM=3000)
C     (Be sure to recompile all users when changing NIAUGM ! )
C     Upper limit for total profile data points for coincident
C     background lines.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external STRIVA, TUNE, KASLAR, MAHIA, HI, BYE
C
      dimension X(*)
C
C               TAB(NTMX)
      dimension TAB(*)
C
      dimension DS(NIAUGM)
C
      call HI ('EUPHEM')
C     !BEG
      call TUNE (LYM)
      LIM = LYM-1
      if(LIM.gt.0) then
C
        K = 0
C----   "LIM" lines captured in detail
        call KASLAR     (X, 1, LIM, NIAUGM, DS, K)
C----   (More wavelengths, up to Lyman edge ?)
        call MAHIA      (      LIM, NIAUGM, DS, K)
C
        do 100 I = 1,K
          call STRIVA   (DS(I), ZERO, WLO, WHI, NTMX, TAB, NT, 'EUPHEM')
  100   continue
C
      end if
C     !END
      call BYE ('EUPHEM')
C
      return
      end
