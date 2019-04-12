      subroutine CLAUDIA
     $(XLM,WAVE,ARR,NW,N,COMP)
C
C     Rudolf Loeser, 1988 Oct 25
C---- Obtains a complete set of "Kurucz" Line opacities at wavelength
C     XLM, from the precomputed (wavelength,depth) array.
C     (This is version 2 of CLAUDIA.)
C     !DASH
      save
C     !DASH
      real*8 ARR, COMP, WAVE, XLM, ZERO
      integer I, INDX, LOOK, N, NOTE, NW
      logical CZERO
C     !DASH
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
C               NW = KNW or NCP
C
C               WAVE(NW), ARR(NW,N), COMP(N)
      dimension WAVE(*),  ARR(NW,*), COMP(*)
C     !EJECT
C
      call HI ('CLAUDIA')
C     !BEG
      call NAUGHTD      (COMP, 1, N, CZERO)
C
      if(CZERO) then
        call LOOKSD     (WAVE, NW, ZERO, XLM, INDX, NOTE, LOOK)
        if(LOOK.eq.1) then
          if(NOTE.eq.1) then
            call MOVED  (ARR(INDX,1), NW, N, COMP, 1, N)
          else
            do 100 I = 1,N
              call LINT (WAVE(INDX  ), ARR( INDX,   I),
     $                   WAVE(INDX+1), ARR((INDX+1),I), 1, XLM, COMP(I))
  100       continue
          end if
        else if(LOOK.eq.4) then
          call MOVED    (ARR( 1,1), NW, N, COMP, 1, N)
        else if((LOOK.eq.3).or.(LOOK.eq.2)) then
          call MOVED    (ARR(NW,1), NW, N, COMP, 1, N)
C
        else
          write (MSSLIN(1),101) LOOK
  101     format('LOOK =',I12,', which is not 1, 2, 3, or 4.')
          call HALT     ('CLAUDIA', 1)
        end if
      end if
C     !END
      call BYE ('CLAUDIA')
C
      return
      end
