      subroutine ADAPA
     $(WVL,OPAC,CSF,N,TIT)
C
C     Rudolf Loeser, 1981 Aug 28
C---- Dumps for continuous emission calculations.
C     !DASH
      save
C     !DASH
      real*8 CSF, OPAC, WVL
      integer I, LUEO, N
      character TIT*(*)
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
C               OPAC(N), CSF(N)
      dimension OPAC(*), CSF(*)
C
      call HI ('ADAPA')
C     !BEG
      call LINER (3, LUEO)
      write (LUEO,100) TIT,WVL
  100 format(' ','Continuum ',A,' details',10X,'WVL=',1PE16.8//
     $       ' ',16X,'OPAC',13X,'CSF')
      call LINER (1, LUEO)
      write (LUEO,101) (I,OPAC(I),CSF(I),I=1,N)
  101 format(5(' ',I4,1P2E16.8/))
C     !END
      call BYE ('ADAPA')
C
      return
      end
