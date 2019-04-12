      subroutine SOPHRON
     $(IU,IL,NL,N,RHO,AIJ,NED,DRHO,CIJ,CAU1,ORHO,KILROY)
C
C     Rudolf Loeser, 1977 Mar 14
C---- Does Rho-editing (anticipating the next LYMAN calculation).
C     !DASH
      save
C     !DASH
      real*8 AIJ, CAU1, CIJ, DRHO, ORHO, RHO
      integer IL, IU, KODE, N, NED, NL, jummy
      logical DUMP, KILROY, LEGEND, NEDOK
C     !DASH
      external EUTROPA, ECHEL, EDMYG, MOVE1, HI, BYE
C
C               RHO(N), CIJ(N,NL**2), AIJ(NL,NL), CAU1(N,NL), ORHO(N)
      dimension RHO(*), CIJ(*),       AIJ(*),     CAU1(N,*),  ORHO(*)
C
      data DUMP /.true./
C
      call HI ('SOPHRON')
C     !BEG
      NEDOK = (NED.gt.0).and.(NED.lt.N)
C
      if((IL.eq.1).and.NEDOK) then
        if(KILROY) then
C----     Get CAU1 values
          KILROY = .false.
          call EUTROPA (N, NL, CIJ, AIJ, CAU1)
          LEGEND = .true.
        end if
C
C----   Check for and do editing
        KODE = 0
        call EDMYG     (N, IU, IL, NED, DRHO, RHO, CAU1, 1, KODE)
        if(KODE.eq.1) then
          call MOVE1   (RHO, N, ORHO)
          call EDMYG   (N, IU, IL, NED, DRHO, RHO, CAU1, 2, jummy)
          if(DUMP) then
            call ECHEL (LEGEND, 'SOPHRON', N, IU, IL, NED, DRHO, ORHO,
     $                  RHO, CAU1(1,IU))
          end if
        end if
C
      end if
C     !END
      call BYE ('SOPHRON')
C
      return
      end
