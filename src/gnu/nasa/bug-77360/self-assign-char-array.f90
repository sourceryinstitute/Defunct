character(1), allocatable :: linelist(:)
linelist = ['a']
if (.false.) linelist(1) = linelist(1)
linelist(1) = linelist(1)
print *,linelist(1)
end
