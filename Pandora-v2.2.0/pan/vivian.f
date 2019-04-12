      subroutine VIVIAN
     $(XLM,WAVE,ARR,KWA,N,COMP)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Obtains the complete set of Averaged Line opacities at wavelength
C     XLM, from the precomputed (wavelength,depth) array.
C     !DASH
      save
C     !DASH
      real*8 ARR, COMP, WAVE, XLM, ZERO
      integer I, INDX, KWA, LOOK, N, NOTE
      logical CZERO
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external NAUGHTD, LOOKSD, MOVED, LINT, HALT, HI, BYE
C
C               WAVE(KWA), ARR(KWA,N), COMP(N)
      dimension WAVE(*),   ARR(KWA,*), COMP(*)
C     !EJECT
C
      call HI ('VIVIAN')
C     !BEG
      call NAUGHTD      (COMP, 1, N, CZERO)
C
      if(CZERO) then
        call LOOKSD     (WAVE, KWA, ZERO, XLM, INDX, NOTE, LOOK)
        if(LOOK.eq.1) then
          if(NOTE.eq.1) then
            call MOVED  (ARR(INDX,1), KWA, N, COMP, 1, N)
          else
            do 100 I = 1,N
              call LINT (WAVE(INDX  ), ARR( INDX   ,I),
     $                   WAVE(INDX+1), ARR((INDX+1),I), 1, XLM, COMP(I))
  100       continue
          end if
        else if(LOOK.eq.4) then
          call MOVED    (ARR(  1,1), KWA, N, COMP, 1, N)
        else if((LOOK.eq.3).or.(LOOK.eq.2)) then
          call MOVED    (ARR(KWA,1), KWA, N, COMP, 1, N)
C
        else
          write (MSSLIN(1),101) LOOK
  101     format('LOOK =',I12,', which is not 1, 2, 3, or 4.')
          call HALT     ('VIVIAN', 1)
        end if
      end if
C     !END
      call BYE ('VIVIAN')
C
      return
      end
