      subroutine HEGRE
     $(N,Z,TE,TCO,HND,CONI,CON,LU)
C
C     Rudolf Loeser, 1995 May 17
C---- Sets up CONI in CON, if needed.
C     (This is version 2 of HEGRE.)
C     !DASH
      save
C     !DASH
      real*8 CON, CONI, HND, TCO, TE, Z
      integer I, LU, N
      logical CONIZ
C     !DASH
      external NAUGHTD, ABJECT, LINER, MOVE1, CHECKER, WENDY, HI, BYE
C
C               Z(N), TE(N), HND(N), CONI(N), CON(N), TCO(N)
      dimension Z(*), TE(*), HND(*), CONI(*), CON(*), TCO(*)
C
      call HI ('HEGRE')
C     !BEG
      call NAUGHTD    (CONI, 1, N, CONIZ)
      if(.not.CONIZ) then
C
        if(LU.gt.0) then
          call ABJECT (LU)
          write (LU,100)
  100     format(' ','NCO comparison.'//
     $           ' ',77X,'Computed',11X,'Input'/
     $           ' ',20X,'Z',14X,'TE',13X,'TCO',13X,'HND',13X,'NCO',
     $               13X,'NCO')
          call LINER  (1, LU)
          write (LU,101) (I,Z(I),TE(I),TCO(I),HND(I),CON(I),CONI(I),
     $                    I=1,N)
  101     format(5(' ',I5,1P6E16.8/))
          call LINER  (1, LU)
          write (LU,102)
  102     format(' ','The input values will be used instead of the ',
     $               'computed ones.')
        end if
C
        call MOVE1    (CONI, N, CON)
        call CHECKER  (CON, 1, N, 'Input N(CO)')
        call WENDY    (CON, 1, N, 8, 'HEGRE')
C
      end if
C     !END
      call BYE ('HEGRE')
C
      return
      end
