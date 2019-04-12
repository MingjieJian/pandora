      subroutine TAFFY
     $(IU,IL,N,Z,K,DL,XJNU,NO,ZO,KO,DLO,XJNUO,LU,INTZ)
C
C     Rudolf Loeser, 1980 Jun 06
C---- Prints input Jnu values for P.R.D.
C     !DASH
      save
C     !DASH
      real*8 DEL, DL, DLO, XJNU, XJNUO, Z, ZO
      integer IL, INTZ, IU, K, KEQD, KEQZ, KO, LU, N, NO
      logical FLAG, NZJ
      character BLANK*1, TIT*23
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER, DASHER, ARISOD, VECOUT, SULFUR, NAUGHTD, HI, BYE
C
C               Z(N), DL(K), ZO(NO), DLO(KO), XJNU(N,K), XJNUO(NO,KO)
      dimension Z(*), DL(*), ZO(*),  DLO(*),  XJNU(*),   XJNUO(*)
C
      data DEL /1.D-8/
C     !EJECT
C
      call HI ('TAFFY')
C     !BEG
      if(LU.gt.0) then
        call LINER    (3, LU)
        call DASHER   (LU)
        call LINER    (1, LU)
        write (LU,100) IU,IL
  100   format(' ','Input values of PRD Jnu for transition (',I2,'/',I2,
     $             ').',64X,'(Option JNUPRNT)')
C
        KEQZ = 1
        if(INTZ.eq.1) then
          call ARISOD (Z, N, ZO, NO, DEL, KEQZ)
        end if
        if(KEQZ.eq.0) then
          call LINER  (1, LU)
          write (LU,101)
  101     format(' ','Z-interpolation was required')
          call VECOUT (LU, ZO, NO, 'Previous Z')
          call VECOUT (LU, Z,  N,  'Current Z')
        end if
C
        call ARISOD   (DL, K, DLO, KO, DEL, KEQD)
        if(KEQD.eq.0) then
          call LINER  (1, LU)
          write (LU,104)
  104     format(' ','DL-interpolation was required')
          call VECOUT (LU, DLO, KO, 'Previous DL')
          call VECOUT (LU, DL,  K,  'Current DL')
        end if
C
        FLAG = (KEQZ.eq.0).or.(KEQD.eq.0)
        if(FLAG) then
          call SULFUR (LU, NO, KO, XJNUO, DLO, 'Jnu as read')
        end if
C
        call NAUGHTD  (XJNU, 1, (N*K), NZJ)
        if(.not.NZJ) then
          TIT = BLANK
          if(FLAG) then
            TIT = 'Jnu after interpolation'
          end if
          call SULFUR (LU, N, K, XJNU, DL, TIT)
        end if
      end if
C     !END
      call BYE ('TAFFY')
C
      return
      end
