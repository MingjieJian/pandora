      subroutine BINNA
     $(NW,WAVE,N,YAYB,XCBL)
C
C     Rudolf Loeser, 1984 Aug 14
C---- Reads mean intensities, for OSIRIS.
C     (This is version 2 of BINNA.)
C     !DASH
      save
C     !DASH
      real*8 WAVE, XCBL, YAYB
      integer J, KKJNU, N, NW
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(13),KKJNU )
C     !DASH
      external LAMAR, MOVE1, ZERO1, HI, BYE
C
C               WAVE(NW), YAYB(N,NW), XCBL(Miklen)
      dimension WAVE(*),  YAYB(N,*),  XCBL(*)
C
C
      call HI ('BINNA')
C     !BEG
      do 100 J = 1,(NW-1)
        call LAMAR (WAVE(J), 0, XCBL)
        call MOVE1 (XCBL(KKJNU), N, YAYB(1,J))
  100 continue
      call ZERO1   (YAYB(1,NW), N)
C     !END
      call BYE ('BINNA')
C
      return
      end
