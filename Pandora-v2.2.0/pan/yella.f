      subroutine YELLA
     $(XLM,ITAU,DUMP,DMPI,CALLER)
C
C     Rudolf Loeser, 2003 Dec 02
C---- Sets up dump for CO-opacity calculation.
C     !DASH
      save
C     !DASH
      real*8 XLM
      integer ITAU, NDW
      logical DMPI, DUMP
      character CALLER*(*)
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
      equivalence (KZQ(  1),NDW  )
C     !DASH
      external DACAPO, MESHED, HI, BYE
C
      call HI ('YELLA')
C     !BEG
      DMPI = (ITAU.eq.NDW).and.DUMP
      if(DMPI) then
        call DACAPO (XLM)
        call MESHED (CALLER, 2)
      end if
C     !END
      call BYE ('YELLA')
C
      return
      end
