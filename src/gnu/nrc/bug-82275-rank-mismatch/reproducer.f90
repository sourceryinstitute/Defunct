!! Associating a name with a reduced-dimension section of a
!! multidimensional array precludes subsequent use of the name
!! with the appropriately reduced dimensionality and instead
!! requires use of the (invalid) full set of original dimensions.
!! It seems that this only occurs in the presence of type guarding:
  type component
  end type
  type container
    class(component), allocatable :: component_array(:,:)
  end type
  type(container) bag
  type(component) section_copy
  allocate(component::bag%component_array(1,1))
  select type(associate_name=>bag%component_array(1,:))
    type is (component)
      section_copy = associate_name(1)  ! gfortran 5,6,7,8 reject valid
      section_copy = associate_name(1,1)! gfortran 5,6,7,8 accept invalid
  end select
end
