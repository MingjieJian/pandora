      subroutine LARGO
     $(N,XMX,XMN,RAT,MATLEG,RATMIN)
C
C     Rudolf Loeser, 1983 Aug 15
C---- Merges matrix range data into storage.
C     !DASH
      save
C     !DASH
      real*8 RAT, RATMIN, XMN, XMX
      integer I, II, IP, J, N
      character MATLEG*(*)
C     !COM
C---- MATMAG      as of 1989 Jan 25
      integer     MSTMAX
      parameter   (MSTMAX=10)
C     (Remember to recompile all users when changing MSTMAX)
      integer     MATKNT,NMAT,MATSNO
      real*8      ELLRGE,ELSMLL,ELRNGE
      character   MATNAM*50
      dimension   ELLRGE(MSTMAX),ELSMLL(MSTMAX),NMAT(MSTMAX),
     $            ELRNGE(MSTMAX),MATNAM(MSTMAX)
      common      /MATMAG1/ MATNAM
      common      /MATMAG2/ MATKNT,NMAT
      common      /MATMAG3/ MATSNO
      common      /MATMAG4/ ELLRGE,ELSMLL,ELRNGE
C     Elements range characteristics of the MSTMAX most extreme
C     matrices, (collected when MAMAS=1).
C     .
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic min
C
      call HI ('LARGO')
C     !BEG
      if(MATKNT.eq.0) then
        II = 1
      else
        II = MATKNT+1
        do 100 I = 1,MATKNT
          if(RAT.gt.ELRNGE(I)) then
            II = I
            goto 101
          end if
  100   continue
  101   continue
      end if
C
      IP = II+1
      if(IP.le.MSTMAX) then
        J = MSTMAX
        do 102 I = IP,MSTMAX
          J = J-1
          NMAT  (J+1) = NMAT(J)
          ELLRGE(J+1) = ELLRGE(J)
          ELSMLL(J+1) = ELSMLL(J)
          ELRNGE(J+1) = ELRNGE(J)
          MATNAM(J+1) = MATNAM(J)
  102   continue
      end if
C
      NMAT  (II) = N
      ELLRGE(II) = XMX
      ELSMLL(II) = XMN
      ELRNGE(II) = RAT
      MATNAM(II) = MATLEG
C
      MATKNT = min((MATKNT+1),MSTMAX)
      RATMIN = ELRNGE(MATKNT)
C     !END
      call BYE ('LARGO')
C
      return
      end
