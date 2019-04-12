      subroutine SUMAC
     $(WK,K,N,KIS,KIL,WN)
C
C     Rudolf Loeser, 1968 Jun 06 (revised 2000 Jan 20)
C---- SUMAC expands the K*K input matrix WK into an N*N output matrix
C     WN, by setting WN(i,j) = WK(i,j) for 1 .le. i,j .le. K; while
C     inserting (KIS-1) columns of zeroes between the first two columns,
C     and inserting (KIS-1) rows of zeroes between the first two rows.
C     All other terms of WN are set to zero.
C     (Note that KIL .gt. KIS, that KIL .le. N+1, and that K = KIL-KIS.)
C     !DASH
      save
C     !DASH
      real*8 WK, WN, ZERO
      integer IK, IN, JK, JN, K, KIL, KIS, N
      logical ISELECT, JSELECT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               WK(K,K), WN(N,N)
      dimension WK(K,*), WN(N,*)
C
      call HI ('SUMAC')
C     !BEG
      JK = 0
      do 101 JN = 1,N
        JSELECT = (JN.eq.1).or.((JN.gt.KIS).and.(JN.lt.KIL))
        if(JSELECT) then
          JK = JK+1
        end if
C
        IK = 0
        do 100 IN = 1,N
          ISELECT = (IN.eq.1).or.((IN.gt.KIS).and.(IN.lt.KIL))
          if(ISELECT) then
            IK = IK+1
          end if
C
          if(JSELECT.and.ISELECT) then
            WN(IN,JN) = WK(IK,JK)
          else
            WN(IN,JN) = ZERO
          end if
  100   continue
  101 continue
C     !END
      call BYE ('SUMAC')
C
      return
      end
