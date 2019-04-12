      subroutine HEGRI
     $(METEP,KOLEV,NL,N,RK1W,RKWO)
C
C     Rudolf Loeser, 1987 Nov 05
C---- Checks, and adjusts,
C     Lyman EP1, EP2 calculation control parameters.
C     !DASH
      save
C     !DASH
      real*8 RK1W, RKWO
      integer KOLEV, LUEO, METEP, N, NL
      logical KILROY
      character LAB*6, LABO*5
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external MUSHED, HALT, AKALON, MASHED, HI, BYE
C
C               RK1W(N), RKWO(N)
      dimension RK1W(*), RKWO(*)
C
      call HI ('HEGRI')
C     !BEG
      if((KOLEV.lt.1).or.(KOLEV.gt.NL)) then
        write (MSSLIN(1),100) KOLEV
  100   format('The input value KOLEV =',I5,' is invalid.')
        call HALT   ('HEGRI', 1)
      end if
C
      KILROY = .true.
C
      if((METEP.eq.3).and.(KOLEV.ne.1)) then
        call MUSHED ('HEGRI', 3, KILROY)
        write (LUEO,101) KOLEV
  101   format(' ','KOLEV =',I5,' is not permitted when METEP=3 ',
     $             '-- changed to METEP=0.')
        METEP  = 0
      end if
C
      if((METEP.lt.0).or.(METEP.gt.3)) then
        write (MSSLIN(1),102) METEP
  102   format('The input value METEP =',I5,' is invalid.')
        call HALT   ('HEGRI', 1)
      end if
C
      if(KOLEV.lt.10) then
        write (LAB ,103) 'RKWT',KOLEV
        write (LABO,103) 'RKW' ,KOLEV
  103   format(A,I1)
      else
        write (LAB ,104) 'RKWT',KOLEV
        write (LABO,104) 'RKW' ,KOLEV
  104   format(A,I2)
      end if
C
      call AKALON   (KILROY, 'HEGRI', N, RK1W, LAB, RKWO, LABO)
C
      if(.not.KILROY) then
        call MASHED ('HEGRI')
      end if
C     !END
      call BYE ('HEGRI')
C
      return
      end
