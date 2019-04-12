      subroutine HAMRO
     $(IQDT2,IQND2,NDT,XLDT,YLDT,ADT,ALBDT,IPNT,WORK)
C
C     Rudolf Loeser, 2002 Jan 11
C---- Checks, and massages, Type-2 dust temperature calculation input.
C     !DASH
      save
C     !DASH
      real*8 ADT, ALBDT, DELTA, WORK, XLDT, YLDT, ZERO
      integer IPNT, IQDT2, IQND2, KCHCK, KDIST, KSORT, NDT
      logical DISTERR
C     !COM
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
      equivalence (DLIT( 1),ZERO  )
C
      common      /JUDGD/ DELTA
C     !DASH
      external JUDGED, SORTDD, SORT, ORDERD, REVERSD, HALT, HI, BYE
C
C               XLDT(NDT), YLDT(NDT), WORK(NDT), ALBDT(NDT), IPNT(NDT),
      dimension XLDT(*),   YLDT(*),   WORK(*),   ALBDT(*),   IPNT(*),
C
C               ADT(NDT)
     $          ADT(*)
C     !EJECT
C
      call HI ('HAMRO')
C     !BEG
      if((IQDT2.gt.0).and.(IQND2.gt.0).and.(NDT.gt.0)) then
        DELTA = ZERO
        call SORTDD       (XLDT, NDT, JUDGED, KSORT, KDIST)
C
        DISTERR = (KSORT.ne.0).and.(KDIST.eq.0)
        if(.not.DISTERR) then
C
          if(KSORT.eq.0) then
            call SORT     (XLDT, NDT, IPNT, 'Type-2 Dust wavelengths')
            call SORTDD   (XLDT, NDT, JUDGED, KCHCK, KDIST)
            if(KCHCK.eq.0) then
              write (MSSLIN(1),100)
  100         format('XLDT is still not in sorted order?')
              call HALT   ('HAMRO', 1)
            else
              DISTERR = KDIST.eq.0
            end if
            if(.not.DISTERR) then
              call ORDERD (YLDT , IPNT, NDT, WORK)
              call ORDERD (ADT  , IPNT, NDT, WORK)
              call ORDERD (ALBDT, IPNT, NDT, WORK)
              KSORT = 1
            end if
          end if
        end if
C
        if(.not.DISTERR) then
          if(KSORT.eq.1) then
            call REVERSD  (XLDT , 1, NDT)
            call REVERSD  (YLDT , 1, NDT)
            call REVERSD  (ADT  , 1, NDT)
            call REVERSD  (ALBDT, 1, NDT)
          end if
C
        else
          write (MSSLIN(1),101)
  101     format('Bad XLDT table for dust temperature recalculation.')
          write (MSSLIN(2),102)
  102     format('All elements of XLDT must be distinct.')
          call HALT       ('HAMRO',2)
        end if
      end if
C     !END
      call BYE ('HAMRO')
C
      return
      end
