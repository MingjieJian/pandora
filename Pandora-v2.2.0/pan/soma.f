      subroutine SOMA
     $(IU,IL,TE,SIGMA)
C
C     Rudolf Loeser, 2006 Dec 05
C---- Retrieves SIGMA for AMOS.
C     !DASH
      save
C     !DASH
      real*8 SIG, SIGL, SIGMA, SL, T, TE, TEL, TEMP, TL
      integer I, IL, IRET, IU, JL, JS, JU
      logical KILROY
C     !DASH
      external PARINT, HI, BYE
C
      dimension T(9), TL(9), SIG(9,5,4), SIGL(9,5,4), TEMP(4)
C
      data T /2.5D3, 5.D3, 7.5D3, 1.D4, 1.5D4, 2.D4, 3.D4, 4.D4, 5.D4/
C
      data (SIG(I,1,1),I=1,9) /9*1.D0/
      data (SIG(I,2,1),I=1,9) /
     $ 0.4601D0, 0.4602D0, 0.4919D0, 0.5277D0, 0.5991D0,
     $ 0.6723D0, 0.833D0, 1.008D0, 1.187D0/
      data (SIG(I,3,1),I=1,9) /
     $ 0.23441D0, 0.2528D0, 0.26407D0, 0.2777D0, 0.31438D0,
     $ 0.35728D0, 0.44389D0, 0.52198D0, 0.5892D0/
      data (SIG(I,4,1),I=1,9) /
     $ 0.103713D0, 0.114159D0, 0.133587D0, 0.156571D0, 0.201076D0,
     $ 0.23884D0, 0.29624D0, 0.337371D0, 0.367812D0/
      data (SIG(I,5,1),I=1,9) /
     $ 0.101863D0, 0.137185D0, 0.162184D0, 0.182982D0, 0.214784D0,
     $ 0.237763D0, 0.267798D0, 0.286489D0, 0.298316D0/
C
      data (SIG(I,1,2),I=1,9) /9*1.D0/
      data (SIG(I,2,2),I=1,9) /9*1.D0/
      data (SIG(I,3,2),I=1,9) /
     $ 46.52667D0, 53.90933D0, 59.87467D0, 65.888D0, 79.27733D0,
     $ 94.22267D0, 126.296D0, 156.44D0, 188.9893D0/
      data (SIG(I,4,2),I=1,9) /
     $ 20.4064D0, 22.35213D0, 25.7424D0, 29.89173D0, 38.3088D0,
     $ 45.92333D0, 58.61773D0, 68.7552D0, 76.96D0/
      data (SIG(I,5,2),I=1,9) /
     $ 19.63627D0, 27.39453D0, 32.61573D0, 36.64667D0, 42.41893D0,
     $ 46.37707D0, 51.5824D0, 54.99093D0, 57.21707D0/
C
      data (SIG(I,1,3),I=1,9) /9*1.D0/
      data (SIG(I,2,3),I=1,9) /9*1.D0/
      data (SIG(I,3,3),I=1,9) /9*1.D0/
      data (SIG(I,4,3),I=1,9) /
     $ 445.9356D0, 554.8248D0, 676.5504D0, 841.4064D0, 1186.105D0,
     $ 1541.292D0, 2232.261D0, 2865.535D0, 3427.104D0/
      data (SIG(I,5,3),I=1,9) /
     $ 460.0314D0, 672.7176D0, 812.7804D0, 924.5268D0, 1096.332D0,
     $ 1225.695D0, 1416.043D0, 1554.445D0, 1657.129D0/
C     !EJECT
      data (SIG(I,1,4),I=1,9) /9*1.D0/
      data (SIG(I,2,4),I=1,9) /9*1.D0/
      data (SIG(I,3,4),I=1,9) /9*1.D0/
      data (SIG(I,4,4),I=1,9) /9*1.D0/
      data (SIG(I,5,4),I=1,9) /
     $ 3991.505D0, 6781.510D0, 9324.826D0, 11366.62D0, 15291.36D0,
     $ 18743.47D0, 24663.71D0, 29608.24D0, 33834.22D0/
C
      data KILROY /.true./
C
      call HI ('SOMA')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        do 102 I = 1,9
          TL(I) = log(T(I))
          do 101 JU = 2,5
            do 100 JL = 1,(JU-1)
              SIGL(I,JU,JL) = log(SIG(I,JU,JL))
  100       continue
  101     continue
  102   continue
        JS = 0
      end if
C
      TEL = log(TE)
      call PARINT (TL, 1, SIGL(1,IU,IL), 1, 9, TEL, SL, 1, IRET,
     $             JS, TEMP)
      SIGMA = exp(SL)
C     !END
      call BYE ('SOMA')
C
      return
      end
