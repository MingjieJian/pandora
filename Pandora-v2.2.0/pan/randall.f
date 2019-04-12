      subroutine RANDALL
     $(ARR,QLL,QAR,N,IZERO)
C
C     Rudolf Loeser, 1988 Jul 19
C---- Writes a line of CRD values or footnotes.
C     (QLL is "Line Components" information as produced by CLACK.)
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, IZERO, N, NF
      logical JAR
      character BLANK*1, QAR*10, QLL*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, NAUGHTD, SETC, ENCODED, HI, BYE
C
C               ARR(N), QAR(16), QLL(16)
      dimension ARR(*), QAR(*),  QLL(*)
C     !EJECT
C
      call HI ('RANDALL')
C     !BEG
      if((N.lt.1).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,', which is not 1 through 8, inclusive.')
        call HALT        ('RANDALL',1)
      end if
C
      call NAUGHTD       (ARR,1,N,JAR)
      if(.not.JAR) then
C
        do 101 I = 1,N
          QAR(I) = BLANK
          if(QLL(I)(10:10).eq.'e') then
            call ENCODED (ARR(I),QAR(I)(2:10),9,7,IZERO,NF)
          else if(QLL(I)(10:10).eq.')') then
            QAR(I) = ' see below'
          end if
  101   continue
C
      else
        call SETC        (QAR,1,N,'          ')
      end if
C     !END
      call BYE ('RANDALL')
C
      return
      end
