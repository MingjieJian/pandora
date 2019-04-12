      subroutine STORK
     $(N,NSHL,WN,CODSRW,WP1,WP2,WM1,WM2,DUMP)
C
C     Rudolf Loeser, 1983 Mar 16
C---- Finds Shell-Ray WN-matrices by interpolation.
C     (This is version 2 of STORK.)
C     !DASH
      save
C     !DASH
      real*8 CODSRW, ONE, TWO, WM1, WM2, WN, WP1, WP2
      integer MM, N, NP, NSHL
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external TATAR, STROKE, DOGON, DUSUN, ARRAVE, KULIN, KUNLUN,
     $         HI, BYE
C
C               WN(N,N,NSHL), WP1(N,N), WP2(N,N), WM1(N,N), WM2(N,N),
      dimension WN(N,N,*),    WP1(*),   WP2(*),   WM1(*),   WM2(*),
C
C               CODSRW(NSHL)
     $          CODSRW(*)
C     !EJECT
C
      call HI ('STORK')
C     !BEG
      NP = 0
      do 100 MM = 1,NSHL
        call TATAR      (NP)
        if(CODSRW(MM).eq.TWO) then
          call STROKE (CODSRW, NSHL, MM, 2)
C
          call DOGON    (WN(1,1,MM-2), (NP+2), WM1, (NP+1))
          call DOGON    (WM1         , (NP+1), WM2, NP    )
          call DUSUN    (WN(1,1,MM+2), (NP-2), WP1, (NP-1))
          call DUSUN    (WP1         , (NP-1), WP2, NP    )
C
          call ARRAVE   (WM2, WP2, WN(1,1,MM), (NP**2))
C
          if(DUMP) then
            call KULIN  (MM, NP, WM1, WM2, WN(1,1,MM), WP2, WP1)
          end if
        end if
  100 continue
C
      NP = 0
      do 101 MM = 1,NSHL
        call TATAR      (NP)
        if(CODSRW(MM).eq.ONE) then
          call STROKE   (CODSRW, NSHL, MM, 1)
C
          call DOGON    (WN(1,1,MM-1), (NP+1), WM1, NP)
          call DUSUN    (WN(1,1,MM+1), (NP-1), WP1, NP)
C
          call ARRAVE   (WM1, WP1, WN(1,1,MM), (NP**2))
C
          if(DUMP) then
            call KUNLUN (MM, NP, WM1, WN(1,1,MM), WP1)
          end if
        end if
  101 continue
C     !END
      call BYE ('STORK')
C
      return
      end
