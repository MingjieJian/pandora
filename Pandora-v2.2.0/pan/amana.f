      subroutine AMANA
     $(TAU,N,XLM,II,KODE)
C
C     Rudolf Loeser, 1981 Nov 03
C---- Computes reduced tables index, for Continuum Jnu calculation.
C     !DASH
      save
C     !DASH
      real*8 TAU, TSM, XLM
      integer II, KODE, LUEO, N
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
      equivalence (RZQ( 21),TSM  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NOTLESS, MESHED, VECOUT, MASHED, HI, BYE
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('AMANA')
C     !BEG
      call NOTLESS  (TAU, N, TSM, II)
      KODE = 1
C
      if((II.eq.N).or.(II.eq.0)) then
        call MESHED ('AMANA', 3)
        write (LUEO,100) XLM,TSM
  100   format(' ','Mean Intensity for Lambda =',1PE20.10//
     $         ' ','TAU too small.',10X,'TSM =',E12.4)
        call VECOUT (LUEO, TAU, N, 'TAU')
        call MASHED ('AMANA')
C
        KODE = 0
      end if
C
C     !END
      call BYE ('AMANA')
C
      return
      end
