      subroutine RAM
     $(DMP1,GAW,INC,N,ANT,PA,PG,PB,OMD,DLC,PQ,AW,PD)
C
C     Rudolf Loeser, 1981 Dec 03
C---- Final normalization of cumulated frequency/angle sums.
C     !DASH
      save
C     !DASH
      real*8 ANT, AW, DLC, OMD, PA, PB, PD, PG, PQ
      integer J, LUEO, N
      logical DMP1, GAW, INC
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external ARRDIV, VECOUT, ARROUT, LINER, HI, BYE
C
C               ANT(N), OMD(N), PA(N,N), PB(N), PG(N,N), DLC(N), AW(N),
      dimension ANT(*), OMD(*), PA(N,*), PB(*), PG(N,*), DLC(*), AW(*),
C
C               PQ(N), PD(N)
     $          PQ(*), PD(*)
C
      call HI ('RAM')
C     !BEG
      if(DMP1) then
        call LINER    (2, LUEO)
        call VECOUT   (LUEO, ANT, N, 'ANT')
      end if
C
      if(GAW) then
        if(DMP1) then
          call VECOUT (LUEO, AW , N, 'AW unnormalized')
        end if
        call ARRDIV   (AW, ANT, AW, N)
        if(DMP1) then
          call VECOUT (LUEO, AW , N, 'AW normalized')
        end if
      end if
C
      call ARRDIV     (PB     , ANT, PB     , N)
      call ARRDIV     (OMD    , ANT, OMD    , N)
      call ARRDIV     (DLC    , ANT, DLC    , N)
      call ARRDIV     (PQ     , ANT, PQ     , N)
      do 100 J = 1,N
        call ARRDIV   (PA(1,J), ANT, PA(1,J), N)
        call ARRDIV   (PG(1,J), ANT, PG(1,J), N)
  100 continue
C
      if(DMP1) then
        call VECOUT   (LUEO, PB,  N,    'PB' )
        call VECOUT   (LUEO, OMD, N,    'OMD')
        call VECOUT   (LUEO, DLC, N,    'DLC')
        call VECOUT   (LUEO, PQ,  N,    'PQ' )
        call ARROUT   (LUEO, PA,  N, N, 'PA' )
        call ARROUT   (LUEO, PG,  N, N, 'PG' )
        if(INC) then
          call VECOUT (LUEO, PD,  N,    'PD' )
        end if
      end if
C     !END
      call BYE ('RAM')
C
      return
      end
