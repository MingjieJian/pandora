      subroutine FACET
     $(A,N,FAC,IBG,BOUND)
C     Rudolf Loeser, 1983 Dec 01
C---- Computes that factor by which the members of array A(N)
C     would have to be multiplied to make the average of
C     log(FAC*min(A)) and log(FAC*max(A)) equal to zero.
C---- If the value of FAC as computed would exceed abs(BOUND),
C     then FAC is set to abs(BOUND)/10.
C---- If the value of FAC*max(A) would exceed abs(BOUND),
C     then FAC is adjusted so that abs(FAC*max(A)) equals abs(BOUND).
C     !DASH
      save
C     !DASH
      real*8 A, AA, AVE, BIG, BND, BNL, BOUND, FAC, ONE, RUO, SML, TEN,
     $       TWO, ZERO
      integer I, IBG, N
      logical UNINIT
C     !DASH
      intrinsic abs,min
C
      dimension A(N)
C
      data ZERO,ONE,TWO,TEN /0.D0, 1.D0, 2.D0, 1.D1/
C     !EJECT
C
C     !BEG
      FAC = ZERO
      IBG = 1
      BND = abs(BOUND)
      if(N.gt.0) then
C----   Find extrema, skipping zeros
        UNINIT = .true.
        do 100 I = 1,N
          AA = abs(A(I))
          if(AA.ne.ZERO) then
            if(UNINIT) then
              UNINIT = .false.
              IBG = I
              BIG = AA
              SML = AA
            else
              if(BIG.lt.AA) then
                IBG = I
                BIG = AA
              end if
              SML = min(SML,AA)
            end if
          end if
  100   continue
        if(.not.UNINIT) then
C----     Compute average of logs of extrema
          AVE = (log(BIG)+log(SML))/TWO
C----     Compute log of BND
          BNL = log(BND)
C----     Compute FAC
          if((-AVE).gt.BNL) then
            FAC = BND/TEN
          else
            FAC = exp(-AVE)
          end if
          RUO = FAC*(BIG/BND)
          if(RUO.gt.ONE) then
            FAC = FAC/RUO
          end if
        end if
      end if
C     !END
C
      return
      end
