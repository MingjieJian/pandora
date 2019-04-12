      subroutine IS_DECREASING
     $(A,INC,N,KODE,J)
C
C     Rudolf Loeser, 2005 Mar 11
C
C---- Checks whether elements of array A (stride INC, length N) are in
C     decreasing order, as further specified by input parameter KODE:
C     if KODE = 1 then  A(I) .lt. A(I-1) is required (i.e. strict);
C        KODE = 0       A(I) .le. A(I-1) is required (i.e. weak).
C
C---- The value of J will be set upon return:
C     J = 0 means: A increases as required;
C     J > 0 means: A(J) failed the test.
C
C     (See also IS_INCREASING.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, INC, INEXT, IPREV, J, KODE, N
      logical STRICT
C     !DASH
C               A(INC,N)
      dimension A(*)
C
C     !BEG
      J = 0
C
      if(N.gt.1) then
C
        STRICT = KODE.eq.1
        INEXT  = 1
        do 100 I = 2,N
          IPREV = INEXT
          INEXT = IPREV+INC
          if(STRICT) then
            if(A(INEXT).ge.A(IPREV)) then
              J = I
              goto 101
            end if
          else
            if(A(INEXT).gt.A(IPREV)) then
              J = I
              goto 101
            end if
          end if
  100   continue
C
  101   continue
C
      end if
C     !END
C
      return
      end
