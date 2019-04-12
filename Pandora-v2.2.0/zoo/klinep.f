      subroutine KLINEP
     $(W,JA,IA,JB,IB,SYM,KODE)
C     Rudolf Loeser, 1970 Dec 04
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 DX, DY, TWO, X, XOF, XSC, Y, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, IA, IB, IR, J, JA, JB, KNH, KNT, KNV, KODE, L, LI, LIA,
     $        LJ, LJA, M
      logical LODEA, LODEB, MODE, MODEP
      character SYM*1, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external  KTESTP, KPLOTP
      intrinsic abs, isign, float, ifix
C
      data TWO /2.E+0/
C
C     !BEG
      IR(X) = (ifix(X))+isign(ifix(TWO*(X-float(ifix(X)))),(ifix(X)))
C
      call KTESTP (W,JA,IA,LODEA)
      call KTESTP (W,JB,IB,LODEB)
      if(LODEA.or.LODEB) then
        LJ  = JB-JA
        LI  = IB-IA
        LJA = abs(LJ)
        LIA = abs(LI)
        if((LJA+LIA).gt.0) then
          if(LJA.lt.LIA) then
            M  = LIA+1
            DY = isign(1,LI)
            DX = float(LJ)/float(LIA)
          else
            M  = LJA+1
            DX = isign(1,LJ)
            DY = float(LI)/float(LJA)
          end if
          X = JA
          Y = IA
          MODE = LODEB
          if(.not.LODEA) then
            X = JB
            Y = IB
            DX = -DX
            DY = -DY
            MODE = LODEA
          end if
          if(KODE.le.0) then
            M = M-2
            X = X+DX
            Y = Y+DY
          end if
C     !EJECT
          if(M.gt.0) then
            do 100 L = 1,M
              I = IR(Y)
              J = IR(X)
              call KPLOTP   (W,J,I,SYM)
              if(.not.MODE) then
                call KTESTP (W,J,I,MODEP)
                if(.not.MODEP) then
                  goto 101
                end if
              end if
              X = X+DX
              Y = Y+DY
  100       continue
  101       continue
          end if
        end if
      end if
C     !END
C
      return
      end
