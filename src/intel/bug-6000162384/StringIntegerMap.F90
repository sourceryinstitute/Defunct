module  PFL_StringIntegerMap_mod
      implicit none

      integer, parameter :: SIZE_KIND =                                        &
     &           max(kind(1),selected_int_kind(18))
      type Unusable
      end type Unusable
      private
      public :: map
      public :: mapIterator
      public :: pair
      type :: iVector
         private
         integer(kind=SIZE_KIND), allocatable :: elements(:)
         integer(kind=SIZE_KIND) :: vsize = 0
      contains
         procedure :: size => ti_size
         procedure :: capacity => ti_capacity
         procedure :: empty => ti_empty

         procedure :: at_size_kind => ti_at_size_kind
         generic :: at => at_size_kind

         procedure :: at_32 => ti_at_32
         generic :: at => at_32


         procedure :: of => ti_of
         procedure :: get_size_kind => ti_get_size_kind
         generic :: get => get_size_kind

         procedure :: get_32 => ti_get_32
         generic :: get => get_32



         procedure :: get_data => ti_get_data

         procedure :: back => ti_back
         procedure :: front => ti_front


         procedure :: set_size_kind => ti_set_size_kind
         generic :: set => set_size_kind

         procedure :: set_32 => ti_set_32
         generic :: set => set_32



         procedure :: copyFromArray => ti_copyfromarray
         generic :: assignment(=) => copyFromArray

         procedure :: push_back => ti_push_back
         procedure :: pop_back => ti_pop_back
         procedure :: insert_size_kind => ti_insert_size_kind
         generic :: insert => insert_size_kind

         procedure :: insert_32 => ti_insert_32
         generic :: insert => insert_32


         procedure :: resize_size => ti_resize_size
         generic :: resize => resize_size

         procedure :: resize_32 => ti_resize_32
         generic :: resize => resize_32

         procedure :: clear => ti_clear
         procedure :: shrink_to_fit => ti_shrink_to_fit

         procedure :: reserve_size_kind => ti_reserve_size_kind
         generic :: reserve => reserve_size_kind

         procedure :: reserve_32 => ti_reserve_32
         generic :: reserve => reserve_32



         procedure :: swap => ti_swap
         procedure :: reset => ti_reset


         procedure, private :: set_capacity => ti_set_capacity
         procedure, private :: grow_to => ti_grow_to
         procedure, private :: downsize=>ti_downsize
      end type iVector
      

      interface iVector
         module procedure ti_new_empty
      end interface iVector

      interface swap
         module procedure ti_swap
      end interface swap


      type :: pair
        character(len=:) , allocatable :: key
        integer  :: value
      contains
        procedure :: pairEqual
        generic :: operator(==) => pairEqual
        procedure :: pairSameKey
        generic :: operator(.sameKey.) => pairSameKey
      end type pair

      interface pair
         module procedure m_newPair
      end interface pair

      type :: tVector
         private
         type(pair), allocatable :: elements(:)
         integer(kind=SIZE_KIND) :: vsize = 0
      contains
         procedure :: size => tt_size
         procedure :: capacity => tt_capacity
         procedure :: empty => tt_empty

         procedure :: at_size_kind => tt_at_size_kind
         generic :: at => at_size_kind

         procedure :: at_32 => tt_at_32
         generic :: at => at_32


         procedure :: of => tt_of
         procedure :: get_size_kind => tt_get_size_kind
         generic :: get => get_size_kind

         procedure :: get_32 => tt_get_32
         generic :: get => get_32



         procedure :: get_data => tt_get_data

         procedure :: back => tt_back
         procedure :: front => tt_front


         procedure :: set_size_kind => tt_set_size_kind
         generic :: set => set_size_kind

         procedure :: set_32 => tt_set_32
         generic :: set => set_32



         procedure :: copyFromArray => tt_copyfromarray
         generic :: assignment(=) => copyFromArray

         procedure :: push_back => tt_push_back
         procedure :: pop_back => tt_pop_back
         procedure :: insert_size_kind => tt_insert_size_kind
         generic :: insert => insert_size_kind

         procedure :: insert_32 => tt_insert_32
         generic :: insert => insert_32


         procedure :: resize_size => tt_resize_size
         generic :: resize => resize_size

         procedure :: resize_32 => tt_resize_32
         generic :: resize => resize_32

         procedure :: clear => tt_clear
         procedure :: shrink_to_fit => tt_shrink_to_fit

         procedure :: reserve_size_kind => tt_reserve_size_kind
         generic :: reserve => reserve_size_kind

         procedure :: reserve_32 => tt_reserve_32
         generic :: reserve => reserve_32



         procedure :: swap => tt_swap
         procedure :: reset => tt_reset

         procedure, private :: set_capacity => tt_set_capacity
         procedure, private :: grow_to => tt_grow_to
         procedure, private :: downsize=>tt_downsize
      end type tVector
      

      interface tVector
         module procedure tt_new_empty
      end interface tVector

      interface swap
         module procedure tt_swap
      end interface swap

      integer(kind=SIZE_KIND), parameter :: UNINITIALIZED = -1

      integer, parameter :: LEFT = 0, RIGHT = 1

      type :: set
        private
        type (Tvector) :: items
        type (Ivector) :: parents
        type (Ivector) :: lefts
        type (Ivector) :: rights
        type (Ivector) :: heights
        integer(kind=SIZE_KIND) :: root = UNINITIALIZED
        integer(kind=SIZE_KIND) :: tsize = 0
        integer(kind=SIZE_KIND) :: next_free = 0
      contains
        procedure :: empty => s_empty
        procedure :: size => s_size
        procedure :: count => s_count
        procedure :: find => s_find
        procedure :: clear => s_clear
        procedure :: insert => s_insert
        procedure :: erase_one => s_erase_one
        procedure :: erase_multi => s_erase_multi
        generic :: erase => erase_one, erase_multi
        procedure :: remove => s_remove
        procedure :: begin => s_begin
        procedure :: end => s_end
!!$#ifdef _DUMP_TREE
        procedure :: dump => s_dump
!!$#endif
        procedure :: deepCopy => s_deepCopy
        generic :: assignment(=) => deepCopy
        procedure :: equalSets
        generic :: operator(==) => equalSets
        procedure :: notEqualSets
        generic :: operator(/=) => notEqualSets
        procedure, private :: get_child
        procedure, private :: set_child
        procedure, private :: set_parent_child
        procedure, private :: find_index
        procedure, private :: update_height
        procedure, private :: rebalance
        procedure, private :: erase_nonleaf
        procedure, private :: advpos
        procedure, private :: rot
      end type set

      type :: s_iterator
        private
        type (set), pointer :: reference => null()
        integer(kind=SIZE_KIND) :: current = UNINITIALIZED
      contains
        procedure :: value => s_value
        procedure :: next => s_next
        procedure :: prev => s_prev
        procedure :: equalIters => s_equalIters
        procedure :: notEqualIters => s_notEqualIters
        generic :: operator(==) => equalIters
        generic :: operator(/=) => notEqualIters
      end type s_iterator

      type :: map
         private
         type(set) :: tree
      contains
         procedure :: empty => m_empty
         procedure :: size => m_size
         procedure, nopass :: max_size => m_max_size

         procedure :: insert_key_value => m_insert_key_value
         procedure :: insert_pair => m_insert_pair
         generic :: insert => insert_key_value
         generic :: insert => insert_pair

         procedure :: of => m_of ! [] operator
         procedure :: at => m_at
         procedure :: erase_one => m_erase_one
         generic :: erase => erase_one
         procedure :: clear => m_clear
         procedure :: get => m_get
         procedure :: set => m_set

         procedure :: begin => m_begin
         procedure :: end => m_end
         procedure :: find => m_find
         
         procedure :: count => m_count
         procedure :: deepCopy => m_deepCopy

      end type map


      type :: mapIterator
        private
        type(s_iterator) :: setIter
        class(map), pointer :: reference
      contains
         procedure :: value => m_value
         procedure :: key => m_key
         procedure :: next => m_next
         procedure :: previous => m_previous
         procedure :: equal => m_iter_equal
         generic :: operator(==) => equal
         procedure :: notEqual => m_iter_not_equal
         generic :: operator(/=) => notEqual
      end type mapIterator



      interface map
         module procedure m_new_map_empty
         module procedure m_new_map_from_pair_array
      end interface map


      contains

! =======================
!  exists - a helper function
! =======================
      logical function exists(pos)
         integer(kind=SIZE_KIND), intent(in) :: pos
         exists = (pos /= UNINITIALIZED)
      end function exists


! =======================
!  opposite - a helper function
! =======================
      integer function opposite(dir)
         integer, intent(in) :: dir
         
         opposite = (1 - dir)
         
      end function opposite


! =======================
!  child
! =======================
      function get_child(this, pos, dir) result(child)
         integer(kind=SIZE_KIND) :: child
         class(set), intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: pos
         integer, intent(in) :: dir

         select case (dir)
         case (LEFT)
            child = this%lefts%at(pos)
         case (RIGHT)
            child = this%rights%at(pos)
         end select

      end function get_child

! =======================
!  set_child
! =======================
      subroutine set_child(this, pos, dir, child)
         class(set), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: pos
         integer, intent(in) :: dir
         integer(kind=SIZE_KIND), intent(in) :: child

         select case (dir)
         case (LEFT)
           if (child == 0) then
           end if
            call this%lefts%set(pos, child)
         case (RIGHT)
            call this%rights%set(pos, child)
         end select

      end subroutine set_child

! =======================
!  set_parent_child
! =======================
      subroutine set_parent_child(this, parent, pos, other)
         class(set), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: parent
         integer(kind=SIZE_KIND), intent(in) :: pos
         integer(kind=SIZE_KIND), intent(in) :: other

         if (this%lefts%at(parent) == pos) then
            call this%set_child(parent, LEFT, other)
         else
            call this%set_child(parent, RIGHT, other)
         end if

      end subroutine set_parent_child

! =======================
!  update_height
! =======================
      subroutine update_height(this, pos) 
         class (set), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: pos

         integer(kind=SIZE_KIND) :: idx
         integer(kind=SIZE_KIND) :: new_height

         new_height = 0

         idx = this%lefts%at(pos)
         if (exists(idx)) then
            new_height = this%heights%at(idx)
         end if

         idx = this%rights%at(pos)
         if (exists(idx)) then
            new_height = max(new_height, this%heights%at(idx))
         end if
         
         new_height = new_height + 1
         call this%heights%set(pos, new_height)

      end subroutine update_height

!=========================================================================

! =======================
!  empty
! =======================
      logical function s_empty(this) result(empty)
      class(set), intent(in) :: this

      empty = (.not. exists(this%root))

      end function s_empty

! =======================
!  size
! =======================
      function s_size(this) result(size)
         integer(kind=SIZE_KIND) :: size
         class(set), intent(in) :: this

         size = this%tsize

      end function s_size

! =======================
!  find
! =======================
      function s_find(this, value) result(find)
      class(set), target, intent(in) :: this
      type(pair) , intent(in) :: value
      type(s_iterator) :: find

# 153

      
      find%reference => this
      associate (c => find%current)
        c = this%find_index(value, .false.)

        if (exists(c)) then
# 166

           if (.not.s_orderEq(                                                     &
     &                    this%items%at(c),value)) then
              c = UNINITIALIZED
           end if



        end if

      end associate

      end function s_find


      logical function s_orderEq(x, y) result(equal)
         type(pair) , intent(in) :: x
         type(pair) , intent(in) :: y

         equal = .not. s_lessThan(x,y) .and.                            &
     &           .not. s_lessThan(y,x)
      end function s_orderEq


! =======================
!  count
! =======================
      function s_count(this, value) result(count)
      integer(kind=SIZE_KIND) :: count
      class(set), target, intent(in) :: this
      type(pair) , intent(in) :: value

      type (s_iterator) :: i

      i = this%find(value)

      if (.not. exists(i%current)) then
         count = 0
      else 
         count = 1
      end if

      end function s_count

! =======================
!  clear
! =======================
      subroutine s_clear(this)
      class(set), intent(inout) :: this

      call this%items%clear()
      call this%parents%clear()
      call this%lefts%clear()
      call this%rights%clear()
      call this%heights%clear()

      this%root = UNINITIALIZED
      this%tsize=0

      end subroutine s_clear

! =======================
!  insert
! =======================
      subroutine s_insert(this, value, unused, isNew, ref)
      class(set), intent(inout) :: this
      type(pair) , intent(in) :: value
      type (Unusable), optional :: unused
      logical, optional, intent(out) :: isNew

      integer(kind=SIZE_KIND), optional, intent(out) :: ref

      integer(kind=SIZE_KIND) :: new
      integer(kind=SIZE_KIND) :: parent
      logical :: eq
# 243


      if (present(unused)) print*,shape(unused)
      
      if (exists(this%root)) then
# 250

        parent = this%find_index(value, .false.)


# 258

        if (exists(parent)) then
           eq = s_orderEq(this%items%at(parent), value)
        else
           eq = .false.
        end if

        if (eq) then
           if (present(ref)) then
              ref = parent
           else
              call this%items%set(parent, value)
           endif
           if (present(isNew)) then
              isNew = .false.
           end if
           return
        endif

        if (present(isNew)) then
           isNew = .true.
        end if

        if (this%next_free == 0) then
           call this%items%push_back(value)
           new = this%items%size()
           call this%heights%push_back(1_SIZE_KIND)
           call this%lefts%push_back(UNINITIALIZED)
           call this%rights%push_back(UNINITIALIZED)
           call this%parents%push_back(parent)
        else
           new = this%next_free
           this%next_free = this%parents%at(new)
           call this%items%set(new, value)
           call this%heights%set(new, 1_SIZE_KIND)
           call this%lefts%set(new, UNINITIALIZED)
           call this%rights%set(new, UNINITIALIZED)
           call this%parents%set(new, parent)
        end if

        if (present(ref)) ref=new

# 308

           if (s_lessThan(value, this%items%at(parent))) then
              call this%lefts%set(parent, new)
           else
              call this%rights%set(parent,new)
           end if

        call this%rebalance(parent, .true.)

      else ! new root
         
        if (this%next_free == 0) then
           call this%items%push_back(value)
           new = this%items%size()
           call this%heights%push_back(1_SIZE_KIND)
           call this%lefts%push_back(UNINITIALIZED)
           call this%rights%push_back(UNINITIALIZED)
           call this%parents%push_back(UNINITIALIZED)
        else
           new = this%next_free
           this%next_free = this%parents%at(new)
           call this%items%set(new, value)
           call this%heights%set(new, 1_SIZE_KIND)
           call this%lefts%set(new, UNINITIALIZED)
           call this%rights%set(new, UNINITIALIZED)
           call this%parents%set(new, UNINITIALIZED)
        end if
        this%root = new

        if (present(ref)) ref = this%root
        if (present(isNew)) then
           isNew = .true.
        end if

      endif

      this%tsize = this%tsize + 1

      end subroutine s_insert

      logical function s_lessThan(x, y) result(less)
         type(pair) , intent(in) :: x
         type(pair) , intent(in) :: y

         less = (x%key)<(y%key)

      contains

! TODO:  possibly this procedure should be inside some sort of #ifdef
         logical function dictionaryLessThan1d(x, y) result(less)
            integer, intent(in) :: x(:)
            integer, intent(in) :: y(:)

            integer(kind=SIZE_KIND) :: i, n

            n = min(size(x),size(y))
            
            do i = 1, n
               less = (x(i) < y(i))
               if (.not. x(i) == y(i)) return
            end do
            
            less = (size(x) < size(y))
            
         end function dictionaryLessThan1d

# 423

      
      end function s_lessThan



         
         

! =======================
!  erase
! =======================
      subroutine s_erase_one(this, iter)
      class(set), intent(inout) :: this
      type(s_iterator), intent(inout) :: iter

      type (s_iterator) :: last

      last = iter
      call last%next()
      call this%erase(iter, last)

      end subroutine s_erase_one


! =======================
!  erase_multi
! =======================
      subroutine s_erase_multi(this, first, last)
      class(set), intent(inout) :: this
      type(s_iterator), intent(inout) :: first
      type(s_iterator), intent(in) :: last

      integer(kind=SIZE_KIND) :: parent
      integer(kind=SIZE_KIND) :: pos

      type (s_iterator) :: iter

      iter = first

      do while (iter /= last)

         pos = iter%current
         call iter%next()

         if (exists(this%rights%at(pos))) then
            call this%erase_nonleaf(pos, 1)
         else if (exists(this%lefts%at(pos))) then
            call this%erase_nonleaf(pos, 0)
         else
            parent = this%parents%at(pos)
            if (exists(parent)) then
               call this%set_parent_child(parent, pos, UNINITIALIZED)
               call this%rebalance(parent, .false.)
            else
               this%root = UNINITIALIZED
            endif
            
! declare this space available
            call this%parents%set(pos, this%next_free)
            this%next_free = pos

         endif
         this%tsize = this%tsize - 1
      end do
      first = last

      return
      end subroutine s_erase_multi

! =======================
!  remove
! =======================
      subroutine s_remove(this, value)
      class(set), target, intent(inout) :: this
      type(pair) , intent(in) :: value
      type(s_iterator) :: it

      it=this%find(value)
      if (it/=this%end()) call this%erase(it)

      end subroutine s_remove

! =======================
!  begin
! =======================
      function s_begin(this) result(begin)
      class(set), target, intent(in) :: this
      type(s_iterator) :: begin

      begin%reference=>this
      call begin%next()

      end function s_begin

! =======================
!  end
! =======================
      function s_end(this) result(end_)
      class(set), target, intent(in) :: this
      type(s_iterator) :: end_

      end_%reference=>this

      end function s_end


! =======================
!  dump
! =======================
      recursive subroutine s_dump(this)
      class(set), intent(in) :: this

      integer(kind=SIZE_KIND) :: i

# 540

      write(*,'(2x,5(1x,a3,2x))') ' # ', 'par', 'lft', 'rht', ' at '


      do i = 1, this%items%size()
# 551

         write(*,'(5(i5,1x))') i,                                              &
     &             this%parents%at(i),                                         &
     &             this%lefts%at(i), this%rights%at(i),                        &
     &             this%heights%at(i)

      end do

      end subroutine s_dump

! =======================
!  find_index
! =======================
      function find_index(this, value, last) result(idx)
      integer(kind=SIZE_KIND) :: idx
      class(set), target, intent(in) :: this
      type(pair) , intent(in) :: value
      logical, intent(in) :: last

      integer (kind=SIZE_KIND) :: child

# 574


      idx = this%root
      if (exists(idx)) then
         do
# 583

            if (.not. last .and. (                                           &
     &         s_orderEq(this%items%at(idx),value))) return

# 592

            child=merge(this%lefts%at(idx), this%rights%at(idx),              &
     &         s_lessThan(value, this%items%at(idx)))

          
            if (.not. exists(child)) return
            idx = child
         end do
      end if

      end function find_index

! =======================
!  rebalance
! =======================
      subroutine rebalance(this, pos, once)
      class(set), intent(inout) :: this
      integer(kind=SIZE_KIND), intent(in) :: pos
      logical, intent(in) :: once

      integer(kind=SIZE_KIND) :: curr, child

      integer :: hl, hr, chl, chr, side, childside
      logical :: unbalanced

      integer(kind=SIZE_KIND), pointer :: pLeft, pRight

      curr = pos

      do while (exists(curr))
         hl = 0
         hr = 0
         pLeft => this%lefts%at(curr)
         if (exists(pLeft)) hl = this%heights%at(pLeft)
         pRight => this%rights%at(curr)
         if (exists(pRight)) hr = this%heights%at(pRight)

         unbalanced = (abs(hl-hr) > 1)

         if (unbalanced) then
            side = merge(LEFT, RIGHT, hl>hr)
            child = this%get_child(curr, side)

            chl=0
            chr=0

            pLeft => this%lefts%at(child)
            if (exists(pLeft)) chl = this%heights%at(pLeft)
            pRight => this%rights%at(child)
            if (exists(pRight)) chr = this%heights%at(pRight)

            if (chr /= chl) then
               childside=merge(0, 1, chl>chr)
               if (side/=childside)                                                &
     &            call this%rot(child,opposite(childside))
               call this%rot(curr, opposite(side))
            endif
         endif
         call this%update_height(curr)


         if (unbalanced.and.once) exit

         curr = this%parents%at(curr)

      end do

      end subroutine rebalance

! =======================
!  erase_nonleaf
! =======================
      subroutine erase_nonleaf(this, pos, side)
      class(set), intent(inout) :: this
      integer(kind=SIZE_KIND), intent(inout) :: pos
      integer, intent(in) :: side

      integer(kind=SIZE_KIND) :: parent, other, child0, child1,           &
     &      otherchild, otherparent, tmp

      parent = this%parents%at(pos)
      other = pos
      call this%advpos(other, side)

      if (side == 0) then
         child0 = this%lefts%at(pos)
         child1 = this%rights%at(pos)
         otherchild = this%lefts%at(other)
      else
         child0 = this%rights%at(pos)
         child1 = this%lefts%at(pos)
         otherchild = this%rights%at(other)
      end if
      otherparent = this%parents%at(other)
      call this%parents%set(other, parent)

      if (exists(parent)) then
        call this%set_parent_child(parent, pos, other)
      else
        this%root = other
      endif
      call this%set_child(other, 1-side, child1)
      if (exists(child1)) call this%parents%set(child1, other)
      if (other == child0) then
        call this%rebalance(other, .false.)
      else
        call this%set_child(other, side, child0)
        call this%parents%set(child0, other)
        call this%set_child(otherparent, 1-side, otherchild)
        if (exists(otherchild)) then
           call this%parents%set(otherchild, otherparent)
        end if
        call this%rebalance(otherparent, .false.)
      endif

      tmp = this%items%size()

! declare this space available
      call this%parents%set(pos, this%next_free)
      this%next_free = pos

      pos = UNINITIALIZED

      end subroutine erase_nonleaf

! =======================
!  advpos
! =======================
      subroutine advpos(this, pos, dir)
      class(set), target, intent(in) :: this
      integer(kind=SIZE_KIND), intent(inout) :: pos
      integer, intent(in) :: dir   ! dir=1 forward, dir=0 backward

      integer(kind=SIZE_KIND) :: prev
      integer(kind=SIZE_KIND) :: child

      if (.not. exists(pos)) then
         if (.not. exists(this%root)) then
            return
         else
            pos = this%root
            do 
               child = this%get_child(pos, 1-dir)
               if (exists(child)) then
                  pos = child
               else
                  exit
               end if
            end do
         end if
      else
         child = this%get_child(pos, dir)
         if (exists(child)) then
            pos = child
            do 
               child = this%get_child(pos, opposite(dir))
               if (exists(child)) then
                  pos = child
               else
                  exit
               end if
            end do
         else
            prev = pos
            pos = this%parents%at(pos)
            
            do while (exists(pos))
               child = this%get_child(pos, dir)
               if (child /= prev) exit
               prev = pos
               pos = this%parents%at(pos)
            end do
         endif
      end if

      end subroutine advpos

! =======================
!  rot - swap pos with one of its children
! =======================
      subroutine rot(this, pos, dir)
      class(set), intent(inout) :: this
      integer(kind=SIZE_KIND), intent(in) :: pos
      integer, intent(in) :: dir
      
      integer(kind=SIZE_KIND) :: parent, child, grandchild

      parent = this%parents%at(pos)
      child = this%get_child(pos, opposite(dir))

      if (exists(child)) then
         grandchild = this%get_child(child, dir)
      else
         grandchild = UNINITIALIZED
      end if

      if (exists(parent)) then
         call this%set_parent_child(parent, pos, child)
      else ! pos must be root; make the child root instead
         this%root = child
      endif

! 'child' is now my parent
      call this%parents%set(pos, child)

! 'grandchild' becomes now my child
      call this%set_child(pos, opposite(dir), grandchild)

! fix up child
      if (exists(child)) then
         call this%parents%set(child, parent)
         call this%set_child(child, dir, pos)
! and fix up grandchild
         if (exists(grandchild)) then
            call this%parents%set(grandchild, pos)
         end if
      end if

      call this%update_height(pos)
      
      if (exists(child)) call this%update_height(child)

      end subroutine rot

!=========================================================================


! =======================
!  value
! =======================
      function s_value(this) result(value)
      class(s_iterator), target, intent(in) :: this
      type(pair) , pointer :: value

      value=>this%reference%items%at(this%current)

      end function s_value

! =======================
!  next
! =======================
      subroutine s_next(this)
      class(s_iterator), intent(inout) :: this

      call this%reference%advpos(this%current, 1)
 
      end subroutine s_next

! =======================
!  prev
! =======================
      subroutine s_prev(this)
      class(s_iterator), intent(inout) :: this

      call this%reference%advpos(this%current,0)
 
      end subroutine s_prev

! =======================
!  equalIters
! =======================
      logical function s_equalIters(this, other)
      class(s_iterator), intent(in) :: this, other

      s_equalIters = this%current == other%current

      end function s_equalIters

! =======================
!  nequal
! =======================
      logical function s_notEqualIters(this, other)
      implicit none
      class(s_iterator), intent(in) :: this, other

      s_notEqualIters = .not. (this == other)
      
      end function s_notEqualIters


! =======================
!  equalSets
! =======================
      logical function equalSets(this, other)
      class(set), target, intent(in) :: this
      class(set), target, intent(in) :: other

      type (s_iterator) :: iter
      type(pair) , pointer :: ptr

      equalSets = .false. ! unless

      if (this%size() /= other%size()) return

      iter = this%begin()
      do while (iter /= this%end())
         ptr => iter%value()
         if (other%count(ptr) == 0) then
            return
         end if
         call iter%next()
      end do
      
      equalSets = .true.

      end function equalSets


! =======================
!  notEqualSets
! =======================
      logical function notEqualSets(this, other)
      class(set), intent(in) :: this, other

      notEqualSets = .not. (this == other)

      end function notEqualSets


! =======================
!   deepCopy (assignment)
! =======================
      subroutine s_deepCopy(this, other)
         class (set), target, intent(out) :: this
         class (set), target, intent(in) :: other

         type (s_iterator) :: iter
         type(pair) , pointer :: ptr


         iter = other%begin()
         do while (iter /= other%end())
            ptr => iter%value()
            call this%insert(ptr)
            call iter%next()
         end do

         this%tsize = other%tsize

      end subroutine s_deepCopy


      function m_newPair(key, value) result(p)
         type (pair) :: p
         character(len=*) , intent(in) :: key
         integer , intent(in) :: value

         p%key=key
         p%value=value

      end function m_newPair

! =======================
!  pairEqual
! =======================
      function pairEqual(this, other) result(equal)
      class(pair), intent(in) :: this, other
      logical :: equal

      equal = this%pairSameKey(other)

      end function pairEqual


! =======================
!  pairSameKey
! =======================
      function pairSameKey(this, other) result(sameKey)
      class (pair), intent(in) :: this
      class (pair), intent(in) :: other

      logical :: sameKey

      sameKey = .not. ((this%key)<(other%key)) .and.                   &
     &          .not. ((other%key)<(this%key))

   contains

! TODO:  possibly this procedure should be inside some sort of #ifdef
         logical function dictionaryLessThan1d(x, y) result(less)
            integer, intent(in) :: x(:)
            integer, intent(in) :: y(:)

            integer(kind=SIZE_KIND) :: i, n
            
            n = min(size(x),size(y))
            
            do i = 1, n
               less = (x(i) < y(i))
               if (.not. x(i) == y(i)) return
            end do
            
            less = (size(x) < size(y))
            
         end function dictionaryLessThan1d


      end function pairSameKey

! This constructor is needed in situations where an empty dictionary needs to be
! passed to a procedure.  Prevents the need of declaring a local variable.
      function m_new_map_empty() result(m)
         type (map) :: m

         if (.false.) print*,shape(m) ! avoid compiler warnings about unused
         
      end function m_new_map_empty


      function m_new_map_from_pair_array(pairs) result(m)
         type (map) :: m
         type (pair), intent(in) :: pairs(:)
         
         integer :: i

         do i = 1, size(pairs)
            call m%insert(pairs(i))
         end do
         
      end function m_new_map_from_pair_array


! =======================
!  empty
! =======================
      logical function m_empty(this) result(isEmpty)
         class (map), intent(in) :: this

         isEmpty = this%tree%empty()

      end function m_empty


! =======================
!  size
! =======================
      function m_size(this) result(size)
         integer(kind=SIZE_kind) :: size
         class (map), intent(in) :: this

         size = this%tree%size()

      end function m_size


! =======================
!  max_size
! =======================
!  limited by 32 bit integer in terms of result
      function m_max_size() result(max_size)
         integer(kind=SIZE_KIND) :: max_size

         max_size = huge(1_SIZE_KIND)

      end function m_max_size


! =======================
!  insert
! =======================
      subroutine m_insert_key_value(this, key, value)
         class (map), intent(inout) :: this
         character(len=*) , intent(in) :: key
         integer , intent(in) :: value

         type (pair) :: p

         p%key=key
         p%value=value

         call this%tree%insert(p)

      end subroutine m_insert_key_value


      subroutine m_insert_pair(this, p)
         class (map), intent(inout) :: this
         type (pair), intent(in) :: p

         call this%tree%insert(p)

      end subroutine m_insert_pair

! =======================
!  get
! =======================
      function m_get(this, key, value) result(res)
      class(map), target, intent(in) :: this
      character(len=*)  :: key
      integer , pointer, intent(out) :: value
      logical :: res
      type(pair) :: p
      type(s_iterator) :: it

      type(pair), pointer :: q


      p%key=key
      it=this%tree%find(p)
      res= (it/=this%tree%end())

      if (res) then
         q => it%value()
         value => q%value
      end if
# 112

      return
      end function m_get

! =======================
!  set
! =======================
      subroutine m_set(this, key, value)
      class(map), intent(inout) :: this
      character(len=*) , intent(in) :: key
      integer , intent(in) :: value
      type(pair) :: p

      p%key=key
      p%value=value

      call this%tree%insert(p)
      return

      end subroutine m_set

! =======================
!  of - grows map if key does not exist
!  Analog of C++ [] operator.
! =======================
      function m_of(this, key) result(res)
      class(map), target, intent(inout) :: this
      character(len=*) , intent(in) :: key
# 142

      integer , pointer :: res

      type(pair) :: p

      integer(kind=SIZE_KIND) :: ref
# 150

      logical :: isNew

      type(pair), pointer :: q


      p%key=key
      call this%tree%insert(p, ref=ref, isNew=isNew)
      if (.not. isNew) then

         q => this%tree%items%at(ref) 
# 163

         res=>q%value

# 172

      else
         res => null()
      end if
      return
      end function m_of

! =======================
!  at
! =======================
      function m_at(this, key) result(res)
      class(map), target, intent(in) :: this
      character(len=*) , intent(in) :: key
# 187

      integer , pointer :: res


      type (mapIterator) :: iter

      iter = this%find(key)

      if (iter%setIter%current == UNINITIALIZED) then
! throw exception
         res => null()
         return
      end if
# 206


# 210

      res=> iter%value()

      return
      end function m_at



! =======================
!  erase
! =======================
      subroutine m_erase_one(this, iter)
      class(map), intent(inout) :: this
      type(mapIterator), intent(inout) :: iter

      call this%tree%erase(iter%setIter)

      end subroutine m_erase_one


! =======================
!  clear
! =======================
      subroutine m_clear(this)
      class(map), intent(inout) :: this

      call this%tree%clear()

      end subroutine m_clear



! =======================
!  begin
! =======================
      function m_begin(this) result(iter)
         class(map), target, intent(in) :: this
         type (mapIterator) :: iter

         iter%reference => this
         iter%setIter = this%tree%begin()

      end function m_begin


! =======================
!  end
! =======================
      function m_end(this) result(iter)
         class(map), target, intent(in) :: this
         type (mapIterator) :: iter

         iter%reference => this
         iter%setIter = this%tree%end()

      end function m_end


! =======================
!  find
! =======================
      function m_find(this, key) result(iter)
         type (mapIterator) :: iter
         class(map), target, intent(in) :: this
         character(len=*) , intent(in) :: key

         type (pair) :: p

         p%key=key

         iter%reference => this
         iter%setIter = this%tree%find(p)

      end function m_find


! =======================
!  count
! =======================

      function m_count(this, key) result(count)
         integer(kind=SIZE_KIND) :: count
         class(map), intent(in) :: this
         character(len=*) , intent(in) :: key

         type (pair) :: p

         p%key=key

         count = this%tree%count(p)

      end function m_count


! =======================
!  copyFrom
! =======================

      subroutine m_deepCopy(this, original)
         class(map), intent(out) :: this
         class(map), intent(in) :: original

         call this%tree%deepCopy(original%tree)

      end subroutine m_deepCopy
      
! =======================
!  value
! =======================
      function m_value(this) result(res)
         class(mapIterator), target, intent(in) :: this
         integer , pointer :: res

         type(pair), pointer :: p

         p => this%setIter%value()
         res => p%value

      end function m_value

! =======================
!  key
! =======================
      function m_key(this) result(res)
         class(mapIterator), target, intent(in) :: this
         character(len=:) , pointer :: res

         type(pair), pointer :: p

         p => this%setIter%value()
         res => p%key

      end function m_key


! =======================
!  operator(==)
! =======================
      logical function m_iter_equal(this, other) result(equal)
         class(mapIterator), intent(in) :: this
         type(mapIterator), intent(in) :: other

         equal = (this%setIter == other%setIter)

      end function m_iter_equal


! =======================
!  operator(/=)
! =======================
      logical function m_iter_not_equal(this, other)                          &
     &   result(not_equal)
         class(mapIterator), intent(in) :: this
         type(mapIterator), intent(in) :: other

         not_equal = .not. (this == other)
      end function m_iter_not_equal


! =======================
!  next
! =======================
      subroutine m_next(this)
         class(mapIterator), intent(inout) :: this

         call this%setIter%next()
      end subroutine m_next


! =======================
!  previous
! =======================
      subroutine m_previous(this)
         class(mapIterator), intent(inout) :: this

         call this%setIter%prev()
      end subroutine m_previous



! hello type(pair) tt_x

      function tt_new_empty() result(v)
         type (tVector) :: v
         logical, parameter :: flag = .false.
         if (flag) print*,shape(v) ! avoid warning about unused return value
         return
      end function tt_new_empty
      
      
! =======================
!  size
! =======================
      pure function tt_size(this) result(res)
         class(tVector), intent(in) :: this
         integer(kind=SIZE_KIND) :: res
         
         res=this%vsize
         return
      end function tt_size
      
! =======================
!  capacity
! =======================
      pure function tt_capacity(this) result(capacity)
         integer(kind=SIZE_KIND) :: capacity
         class (tVector), intent(in) :: this
         
         if (allocated(this%elements)) then
            capacity = size(this%elements)
         else
            capacity = 0
         end if
         
      end function tt_capacity
      
      
! =======================
!  empty
! =======================
      pure logical function tt_empty(this) result(empty)
         class(tVector), intent(in) :: this
         
         empty = this%vsize==0
         
      end function tt_empty
      
      
! =======================
!  at
! =======================
      function tt_at_size_kind(this, i) result(res)
         class(tVector), target, intent(in) :: this
         integer(KIND=SIZE_KIND), intent(in) :: i
         type(pair) , pointer :: res

         if ((i<=0).or.(i>this%vsize)) stop 'vector::at() out of range'
         res=>this%elements(i)
         return
      end function tt_at_size_kind


      function tt_at_32(this, i) result(res)
         class(tVector), target, intent(in) :: this
         integer, intent(in) :: i
         type(pair) , pointer :: res

!!$         res => this%at_size_kind(int(i,kind=SIZE_KIND))
! workaround for ifort 15.0.3 (no reproducer submitted)
         if ((i<=0).or.(i>this%vsize)) stop 'vector::at() out of range'
         res=>this%elements(i)

      end function tt_at_32



! =======================
!  of
! =======================
      function tt_of(this, i) result(res)
         class(tVector), target, intent(in) :: this
         integer, intent(in) :: i
         type(pair) , pointer :: res

         res=>this%elements(i)
         return
      end function tt_of


! =======================
!  get
! =======================
      function tt_get_size_kind(this, i) result(res)
         class(tVector), target, intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: i
         type(pair)  :: res
         integer(kind=SIZE_KIND) :: idx

         idx=merge(i, this%vsize+i, i>0)
         res=this%elements(idx)

      end function tt_get_size_kind


      function tt_get_32(this, i) result(res)
         class(tVector), target, intent(in) :: this
         integer, intent(in) :: i
         type(pair)  :: res

! This should call get_size_kind(), but there is an ICE for
! gfortran 5.1
         integer(kind=SIZE_KIND) :: idx
         integer(kind=SIZE_KIND) :: i64
         i64 = int(i,kind=SIZE_KIND)
         idx=merge(i64, this%vsize+i64, i64>0)
         res=this%elements(idx)


      end function tt_get_32





! =======================
!  get_data
! =======================
      function tt_get_data(this) result(res)
         class(tVector), target, intent(in) :: this
         type(pair), dimension(:), pointer :: res
         
         res=>this%elements(:this%vsize)
         return
      end function tt_get_data

      
! =======================
!  back
! =======================
      function tt_back(this) result(res)
         class(tVector), target, intent(in) :: this
         type(pair) , pointer :: res
         
         res=>this%elements(this%vsize)
         return
      end function tt_back
      
! =======================
!  front
! =======================
      function tt_front(this) result(res)
         class(tVector), target, intent(in) :: this
         type(pair) , pointer :: res
         
         res=>this%elements(1)
         return
      end function tt_front
      
      
! =======================
!  set
! =======================
      subroutine tt_set_size_kind(this, i, value)
         class(tVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: i
         type(pair) , intent(in) :: value

         integer(kind=SIZE_KIND) :: idx

         idx=merge(i, this%vsize+i, i>0)
         
         
         this%elements(idx)=value
         return
      end subroutine tt_set_size_kind

      subroutine tt_set_32(this, i, value)
         class(tVector), intent(inout) :: this
         integer, intent(in) :: i
         type(pair) , intent(in) :: value

         call this%set(int(i,kind=SIZE_KIND), value)

      end subroutine tt_set_32
      
      
! =======================
!  reset
! =======================
      subroutine tt_reset(this)
         class(tVector), intent(inout) :: this
         
         if (allocated(this%elements)) then
            deallocate(this%elements)
         end if
         this%vsize=0
         return
      end subroutine tt_reset
      
      

! =======================
!  copyFromArray
! =======================
      subroutine tt_copyFromArray(this, array)
         class(tVector), intent(inout) :: this
         type(pair), target, intent(in) :: array(:)
         
         integer(kind=SIZE_KIND) :: n
         
         n = size(array)
         
         call this%reserve(n)
         this%elements(1:n) = array(1:n)
         this%vsize=n
         
         return
      end subroutine tt_copyFromArray

      
      
# 294


# 368

      

      
! =======================
!  push_back
! =======================
      subroutine tt_push_back(this, value)
         class(tVector), intent(inout) :: this
         type(pair) , intent(in) :: value

         call this%grow_to(this%vsize+1)
         call this%resize(this%vsize+1, value)
         return
      end subroutine tt_push_back
      
      
! =======================
!  pop_back
! =======================
      subroutine tt_pop_back(this)
         class(tVector), intent(inout) :: this
         
         call this%downsize(this%vsize-1)
         return
      end subroutine tt_pop_back
      
      
! =======================
!  insert
! =======================
      subroutine tt_insert_size_kind(this, index, value)
         class(tVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: index
         type(pair) , intent(in) :: value
         type(pair)  :: temp
         integer(kind=SIZE_KIND) :: i, n
         
         n = this%vsize
         if (index==n+1) then
           call this%push_back(value)
           return
         endif
         call this%grow_to(this%vsize+1)
         temp=this%elements(n)
         call this%resize(n+1, temp)
         
         do i = n-1, index, -1
          this%elements(i+1)=this%elements(i)
         end do
         
         this%elements(index)=value
         
         return
      end subroutine tt_insert_size_kind

      subroutine tt_insert_32(this, index, value)
         class(tVector), intent(inout) :: this
         integer, intent(in) :: index
         type(pair) , intent(in) :: value
         
         call this%insert(int(index,kind=SIZE_KIND), value)
      end subroutine tt_insert_32

      
! =======================
!  resize
! =======================
      subroutine tt_resize_size(this, newsize, value)
         class(tVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: newsize
         type(pair) , optional, intent(in) :: value
         integer(kind=SIZE_KIND) :: oldSize
         integer(kind=SIZE_KIND) :: i
         
         if (newSize == this%vsize) return
         if (newSize < 0) stop 'vector resize: negative size'
         oldSize=this%vsize
         
         call this%reserve(newSize)
         this%vsize = newSize

         do i = newSize + 1, oldSize
            
         end do
         
         if (present(value) .and. (newsize>oldsize)) then
            do i = oldSize + 1, newSize
               this%elements(i)=value
            end do
         endif
         return
      end subroutine tt_resize_size


      subroutine tt_resize_32(this, newsize, value)
         class(tVector), intent(inout) :: this
         integer, intent(in) :: newsize
         type(pair) , optional, intent(in) :: value

         call this%resize(int(newsize,kind=SIZE_KIND), value)

      end subroutine tt_resize_32



! =======================
!  downsize
! =======================
      subroutine tt_downsize(this, newsize)
         class(tVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: newsize  ! assumes newsize<=size()
         integer(kind=SIZE_KIND) :: i

         if (newsize<this%vsize) then
           do i=newsize+1, this%vsize
              
           end do
           this%vsize=newsize
         endif

         return
      end subroutine tt_downsize
      
      
! =======================
!  clear
! =======================
      subroutine tt_clear(this)
         class(tVector), intent(inout) :: this
         
         call this%downsize(0_SIZE_KIND)
         
         return
      end subroutine tt_clear
      
      
! =======================
!  shrink_to_fit
! =======================

      subroutine tt_shrink_to_fit(this)
         class(tVector), intent(inout) :: this

         if (this%vsize<this%capacity()) then
           call this%set_capacity(this%vsize)
         endif
         return
      end subroutine tt_shrink_to_fit
      

# 556

      
! =======================
!  reserve
! =======================
      subroutine tt_reserve_size_kind(this, capacity)
         class(tVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity

         if (capacity>this%capacity()) then
           call this%set_capacity(capacity)
         endif
         
         return
      end subroutine tt_reserve_size_kind

      subroutine tt_reserve_32(this, capacity)
         class(tVector), intent(inout) :: this
         integer, intent(in) :: capacity
         
         call this%reserve(int(capacity,kind=SIZE_KIND))
         return
      end subroutine tt_reserve_32

      
      
! =======================
!  set_capacity
! =======================
      subroutine tt_set_capacity(this, capacity)
         class(tVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity  ! capacity must be >=0
         type(pair), dimension(:), allocatable :: temp
         integer(kind=SIZE_KIND) :: i

         if (capacity>0) then                     ! capacity>0
           if (.not.allocated(this%elements)) then   ! not allocated
             allocate(this%elements(capacity))
           else                                      ! allocated
             allocate(temp(capacity))
             do i=1, this%vsize
               temp(i)=this%elements(i)
             end do
             call move_alloc(temp, this%elements)
           endif
         else if (allocated(this%elements)) then  ! capacity==0
! Note: vsize must be 0 to reach this point.
            deallocate(this%elements)
         endif

         return
      end subroutine tt_set_capacity

! =======================
!  grow_to
! =======================
      subroutine tt_grow_to(this, capacity)
         class(tVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity

         if (capacity>this%capacity()) then
           call this%set_capacity(max(2*this%vsize, capacity)) ! gives O(n) algorithm for growing vector with push.
         endif
         return
      end subroutine tt_grow_to

! =======================
!  swap
! =======================
      subroutine tt_swap(this, other)
         class(tVector), intent(inout) :: this
         type(tVector), intent(inout) :: other
         type(pair),                                                                &
     &           dimension(:), allocatable :: tmpelementsfer
         integer :: tmpsize
         
         call move_alloc(this%elements, tmpelementsfer)
         call move_alloc(other%elements, this%elements)
         call move_alloc(tmpelementsfer, other%elements)
         tmpsize=this%vsize
         this%vsize=other%vsize
         other%vsize=tmpsize
         return
      end subroutine tt_swap



! hello integer(kind=SIZE_KIND) ti_x

      function ti_new_empty() result(v)
         type (iVector) :: v
         logical, parameter :: flag = .false.
         if (flag) print*,shape(v) ! avoid warning about unused return value
         return
      end function ti_new_empty
      
      
! =======================
!  size
! =======================
      pure function ti_size(this) result(res)
         class(iVector), intent(in) :: this
         integer(kind=SIZE_KIND) :: res
         
         res=this%vsize
         return
      end function ti_size
      
! =======================
!  capacity
! =======================
      pure function ti_capacity(this) result(capacity)
         integer(kind=SIZE_KIND) :: capacity
         class (iVector), intent(in) :: this
         
         if (allocated(this%elements)) then
            capacity = size(this%elements)
         else
            capacity = 0
         end if
         
      end function ti_capacity
      
      
! =======================
!  empty
! =======================
      pure logical function ti_empty(this) result(empty)
         class(iVector), intent(in) :: this
         
         empty = this%vsize==0
         
      end function ti_empty
      
      
! =======================
!  at
! =======================
      function ti_at_size_kind(this, i) result(res)
         class(iVector), target, intent(in) :: this
         integer(KIND=SIZE_KIND), intent(in) :: i
         integer(kind=SIZE_KIND) , pointer :: res

         if ((i<=0).or.(i>this%vsize)) stop 'vector::at() out of range'
         res=>this%elements(i)
         return
      end function ti_at_size_kind


      function ti_at_32(this, i) result(res)
         class(iVector), target, intent(in) :: this
         integer, intent(in) :: i
         integer(kind=SIZE_KIND) , pointer :: res

!!$         res => this%at_size_kind(int(i,kind=SIZE_KIND))
! workaround for ifort 15.0.3 (no reproducer submitted)
         if ((i<=0).or.(i>this%vsize)) stop 'vector::at() out of range'
         res=>this%elements(i)

      end function ti_at_32



! =======================
!  of
! =======================
      function ti_of(this, i) result(res)
         class(iVector), target, intent(in) :: this
         integer, intent(in) :: i
         integer(kind=SIZE_KIND) , pointer :: res

         res=>this%elements(i)
         return
      end function ti_of


! =======================
!  get
! =======================
      function ti_get_size_kind(this, i) result(res)
         class(iVector), target, intent(in) :: this
         integer(kind=SIZE_KIND), intent(in) :: i
         integer(kind=SIZE_KIND)  :: res
         integer(kind=SIZE_KIND) :: idx

         idx=merge(i, this%vsize+i, i>0)
         res=this%elements(idx)

      end function ti_get_size_kind


      function ti_get_32(this, i) result(res)
         class(iVector), target, intent(in) :: this
         integer, intent(in) :: i
         integer(kind=SIZE_KIND)  :: res

! This should call get_size_kind(), but there is an ICE for
! gfortran 5.1
         integer(kind=SIZE_KIND) :: idx
         integer(kind=SIZE_KIND) :: i64
         i64 = int(i,kind=SIZE_KIND)
         idx=merge(i64, this%vsize+i64, i64>0)
         res=this%elements(idx)


      end function ti_get_32





! =======================
!  get_data
! =======================
      function ti_get_data(this) result(res)
         class(iVector), target, intent(in) :: this
         integer(kind=SIZE_KIND), dimension(:), pointer :: res
         
         res=>this%elements(:this%vsize)
         return
      end function ti_get_data

      
! =======================
!  back
! =======================
      function ti_back(this) result(res)
         class(iVector), target, intent(in) :: this
         integer(kind=SIZE_KIND) , pointer :: res
         
         res=>this%elements(this%vsize)
         return
      end function ti_back
      
! =======================
!  front
! =======================
      function ti_front(this) result(res)
         class(iVector), target, intent(in) :: this
         integer(kind=SIZE_KIND) , pointer :: res
         
         res=>this%elements(1)
         return
      end function ti_front
      
      
! =======================
!  set
! =======================
      subroutine ti_set_size_kind(this, i, value)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: i
         integer(kind=SIZE_KIND) , intent(in) :: value

         integer(kind=SIZE_KIND) :: idx

         idx=merge(i, this%vsize+i, i>0)
         
         
         this%elements(idx)=value
         return
      end subroutine ti_set_size_kind

      subroutine ti_set_32(this, i, value)
         class(iVector), intent(inout) :: this
         integer, intent(in) :: i
         integer(kind=SIZE_KIND) , intent(in) :: value

         call this%set(int(i,kind=SIZE_KIND), value)

      end subroutine ti_set_32
      
      
! =======================
!  reset
! =======================
      subroutine ti_reset(this)
         class(iVector), intent(inout) :: this
         
         if (allocated(this%elements)) then
            deallocate(this%elements)
         end if
         this%vsize=0
         return
      end subroutine ti_reset
      
      

! =======================
!  copyFromArray
! =======================
      subroutine ti_copyFromArray(this, array)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND), target, intent(in) :: array(:)
         
         integer(kind=SIZE_KIND) :: n
         
         n = size(array)
         
         call this%reserve(n)
         this%elements(1:n) = array(1:n)
         this%vsize=n
         
         return
      end subroutine ti_copyFromArray

      
      
# 294


# 368

      

      
! =======================
!  push_back
! =======================
      subroutine ti_push_back(this, value)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND) , intent(in) :: value

         call this%grow_to(this%vsize+1)
         call this%resize(this%vsize+1, value)
         return
      end subroutine ti_push_back
      
      
! =======================
!  pop_back
! =======================
      subroutine ti_pop_back(this)
         class(iVector), intent(inout) :: this
         
         call this%downsize(this%vsize-1)
         return
      end subroutine ti_pop_back
      
      
! =======================
!  insert
! =======================
      subroutine ti_insert_size_kind(this, index, value)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: index
         integer(kind=SIZE_KIND) , intent(in) :: value
         integer(kind=SIZE_KIND)  :: temp
         integer(kind=SIZE_KIND) :: i, n
         
         n = this%vsize
         if (index==n+1) then
           call this%push_back(value)
           return
         endif
         call this%grow_to(this%vsize+1)
         temp=this%elements(n)
         call this%resize(n+1, temp)
         
         do i = n-1, index, -1
          this%elements(i+1)=this%elements(i)
         end do
         
         this%elements(index)=value
         
         return
      end subroutine ti_insert_size_kind

      subroutine ti_insert_32(this, index, value)
         class(iVector), intent(inout) :: this
         integer, intent(in) :: index
         integer(kind=SIZE_KIND) , intent(in) :: value
         
         call this%insert(int(index,kind=SIZE_KIND), value)
      end subroutine ti_insert_32

      
! =======================
!  resize
! =======================
      subroutine ti_resize_size(this, newsize, value)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: newsize
         integer(kind=SIZE_KIND) , optional, intent(in) :: value
         integer(kind=SIZE_KIND) :: oldSize
         integer(kind=SIZE_KIND) :: i
         
         if (newSize == this%vsize) return
         if (newSize < 0) stop 'vector resize: negative size'
         oldSize=this%vsize
         
         call this%reserve(newSize)
         this%vsize = newSize

         do i = newSize + 1, oldSize
            
         end do
         
         if (present(value) .and. (newsize>oldsize)) then
            do i = oldSize + 1, newSize
               this%elements(i)=value
            end do
         endif
         return
      end subroutine ti_resize_size


      subroutine ti_resize_32(this, newsize, value)
         class(iVector), intent(inout) :: this
         integer, intent(in) :: newsize
         integer(kind=SIZE_KIND) , optional, intent(in) :: value

         call this%resize(int(newsize,kind=SIZE_KIND), value)

      end subroutine ti_resize_32



! =======================
!  downsize
! =======================
      subroutine ti_downsize(this, newsize)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: newsize  ! assumes newsize<=size()
         integer(kind=SIZE_KIND) :: i

         if (newsize<this%vsize) then
           do i=newsize+1, this%vsize
              
           end do
           this%vsize=newsize
         endif

         return
      end subroutine ti_downsize
      
      
! =======================
!  clear
! =======================
      subroutine ti_clear(this)
         class(iVector), intent(inout) :: this
         
         call this%downsize(0_SIZE_KIND)
         
         return
      end subroutine ti_clear
      
      
! =======================
!  shrink_to_fit
! =======================

      subroutine ti_shrink_to_fit(this)
         class(iVector), intent(inout) :: this

         if (this%vsize<this%capacity()) then
           call this%set_capacity(this%vsize)
         endif
         return
      end subroutine ti_shrink_to_fit
      

# 556

      
! =======================
!  reserve
! =======================
      subroutine ti_reserve_size_kind(this, capacity)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity

         if (capacity>this%capacity()) then
           call this%set_capacity(capacity)
         endif
         
         return
      end subroutine ti_reserve_size_kind

      subroutine ti_reserve_32(this, capacity)
         class(iVector), intent(inout) :: this
         integer, intent(in) :: capacity
         
         call this%reserve(int(capacity,kind=SIZE_KIND))
         return
      end subroutine ti_reserve_32

      
      
! =======================
!  set_capacity
! =======================
      subroutine ti_set_capacity(this, capacity)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity  ! capacity must be >=0
         integer(kind=SIZE_KIND), dimension(:), allocatable :: temp
         integer(kind=SIZE_KIND) :: i

         if (capacity>0) then                     ! capacity>0
           if (.not.allocated(this%elements)) then   ! not allocated
             allocate(this%elements(capacity))
           else                                      ! allocated
             allocate(temp(capacity))
             do i=1, this%vsize
               temp(i)=this%elements(i)
             end do
             call move_alloc(temp, this%elements)
           endif
         else if (allocated(this%elements)) then  ! capacity==0
! Note: vsize must be 0 to reach this point.
            deallocate(this%elements)
         endif

         return
      end subroutine ti_set_capacity

! =======================
!  grow_to
! =======================
      subroutine ti_grow_to(this, capacity)
         class(iVector), intent(inout) :: this
         integer(kind=SIZE_KIND), intent(in) :: capacity

         if (capacity>this%capacity()) then
           call this%set_capacity(max(2*this%vsize, capacity)) ! gives O(n) algorithm for growing vector with push.
         endif
         return
      end subroutine ti_grow_to

! =======================
!  swap
! =======================
      subroutine ti_swap(this, other)
         class(iVector), intent(inout) :: this
         type(iVector), intent(inout) :: other
         integer(kind=SIZE_KIND),                                                                &
     &           dimension(:), allocatable :: tmpelementsfer
         integer :: tmpsize
         
         call move_alloc(this%elements, tmpelementsfer)
         call move_alloc(other%elements, this%elements)
         call move_alloc(tmpelementsfer, other%elements)
         tmpsize=this%vsize
         this%vsize=other%vsize
         other%vsize=tmpsize
         return
      end subroutine ti_swap

end module PFL_StringIntegerMap_mod

