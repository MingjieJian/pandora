      subroutine GRANADA
     $(NO,N,NT,INDEX,A,AREF,THETA,PE,KODE)
C
C     Rudolf Loeser, 1982 Jun 29
C---- Prints functions of (depths, elements).
C     (This is version 2 of GRANADA.)
C     !DASH
      save
C     !DASH
      real*8 A, AREF, PE, THETA, VAL
      integer I, IE, INDEX, IS, KODE, N, NO, NT
      character SIG*1
C     !DASH
      external  LINER, FONSECA, HI, BYE
      intrinsic min
C
C               THETA(N), PE(N), A(N,NT), INDEX(NMT,2), AREF(N,NT)
      dimension THETA(*), PE(*), A(*),    INDEX(*),     AREF(*)
C
      dimension VAL(10), SIG(10)
C
      call HI ('GRANADA')
C     !BEG
      if(NO.gt.0) then
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+10,N)
          call LINER   (2,NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',5X,'Depth',10I11)
          if(KODE.eq.1) then
            call LINER (1,NO)
            write (NO,102) (THETA(I),I=IS,IE)
  102       format(' ','THETA',5X,1P10E11.4)
            write (NO,103) (PE(I),I=IS,IE)
  103       format(' ','PE',8X,1P10E11.4)
          end if
          call LINER   (1,NO)
          call FONSECA (INDEX,A,AREF,VAL,SIG,N,NT,NO,IS,IE)
        if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('GRANADA')
C
      return
      end
