      subroutine LETHE
     $(ABS,NOPAC,N,CONSW)
C
C     Rudolf Loeser, 2003 Aug 21
C---- Edits contribution switches, for SHARI.
C     !DASH
      save
C     !DASH
      real*8 ABS, CONSW, TWO, ZERO
      integer I, N, NOPAC
      logical ALLZ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external NAUGHTD, HI, BYE
C
C               ABS(NOPAC,N), CONSW(NOPAC)
      dimension ABS(NOPAC,*), CONSW(*)
C
      call HI ('LETHE')
C     !BEG
      do 100 I = 1,NOPAC
        if(CONSW(I).gt.ZERO) then
          call NAUGHTD (ABS(I,1), NOPAC, N, ALLZ)
          if(ALLZ) then
            CONSW(I) = TWO
          end if
        end if
  100 continue
C     !END
      call BYE ('LETHE')
C
      return
      end
