      subroutine MANDOLIN
     $(N,TE,XNE,PF,QELSM,IONST,PART,KODE)
C
C     Rudolf Loeser, 2004 Dec 10
C---- Computes a table of Partition Function.
C
C     If PART > 0, then sets all values equal to the constant PART;
C     otherwise, determines values pertaining to (QELSM,IONST):
C          if KODE = 0, then all values will be constant,
C                  = 1, then the values will vary with depth.
C     !DASH
      save
C     !DASH
      real*8 ONE, PART, PE, PF, PRTLM, TE, THETA, XNE, dummy
      integer I, ION, IONST, IRET, JSTCN, KODE, N, NOION
      logical STOP
      character ELE*2, NAME*3, QELSM*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ( 94),NOION)
      equivalence (RZQ( 47),PRTLM)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RECTOR, ELPH, HAMBURG, FRANK, SAGA, TANAGER, HALT, SET1,
     $         HI, BYE
C
C               TE(N), XNE(N), PF(N)
      dimension TE(*), XNE(*), PF(*)
C
      data STOP /.true./
C     !EJECT
C
      call HI ('MANDOLIN')
C     !BEG
      if((NOION.le.0).and.(JSTCN.le.0)) then
        if(PART.gt.0) then
          call SET1        (PF, N, PART)
        else
          ELE = QELSM(1:2)
          ION = IONST
          if(KODE.gt.0) then
            do 101 I = 1,N
              call RECTOR  (TE(I), THETA)
              call ELPH    (XNE(I), TE(I), PE)
              call HAMBURG (ELE, ION+1, THETA, PE, PF(I), dummy, 1)
              if(PF(I).eq.-ONE) then
                write (MSSLIN(1),100) ELE,ION,PART
  100           format('ION = ',A2,I2,', PART =',1PE12.4,
     $                 'Hamburg partition function unknown.')
                call HALT  ('MANDOLIN', 1)
              end if
  101       continue
            call TANAGER   (PF, PF, PRTLM, N)
          else
            if(ION.eq.1) then
              write (NAME,102) ELE
  102         format(A2,' ')
            else
              write (NAME,103) ELE,ION
  103         format(A2,I1)
            end if
            call FRANK     (NAME, 0, dummy, dummy, PART, dummy, IRET)
            if(IRET.gt.0) then
              call SET1    (PF, N, PART)
            else
              call SAGA    (NAME, 'MANDOLIN', STOP)
            end if
          end if
        end if
      end if
C     !END
      call BYE ('MANDOLIN')
C
      return
      end
