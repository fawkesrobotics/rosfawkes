
/***************************************************************************
 *  MultiTypedObjectInterface.cpp - Fawkes BlackBoard Interface - MultiTypedObjectInterface
 *
 *  Templated created:   Thu Oct 12 10:49:19 2006
 *  Copyright  2014  Tim Niemueller
 *
 ****************************************************************************/

/*  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version. A runtime exception applies to
 *  this software (see LICENSE.GPL_WRE file mentioned below for details).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  Read the full text in the LICENSE.GPL_WRE file in the doc directory.
 */

#include <interfaces/MultiTypedObjectInterface.h>

#include <core/exceptions/software.h>

#include <cstring>
#include <cstdlib>

namespace fawkes {

/** @class MultiTypedObjectInterface <interfaces/MultiTypedObjectInterface.h>
 * MultiTypedObjectInterface Fawkes BlackBoard Interface.
 * 
      Describes an object with multiple possible types.
    
 * @ingroup FawkesInterfaces
 */



/** Constructor */
MultiTypedObjectInterface::MultiTypedObjectInterface() : Interface()
{
  data_size = sizeof(MultiTypedObjectInterface_data_t);
  data_ptr  = malloc(data_size);
  data      = (MultiTypedObjectInterface_data_t *)data_ptr;
  data_ts   = (interface_data_ts_t *)data_ptr;
  memset(data_ptr, 0, data_size);
  add_fieldinfo(IFT_UINT32, "obj_id", 1, &data->obj_id);
  add_fieldinfo(IFT_STRING, "type_1", 32, data->type_1);
  add_fieldinfo(IFT_STRING, "type_2", 32, data->type_2);
  add_fieldinfo(IFT_STRING, "type_3", 32, data->type_3);
  add_fieldinfo(IFT_STRING, "type_4", 32, data->type_4);
  unsigned char tmp_hash[] = {0xeb, 0x63, 0x72, 0x82, 0xef, 0xf7, 0x43, 0x42, 0xd0, 0xc3, 0x45, 0x2e, 0x53, 00, 0xd4, 0x4a};
  set_hash(tmp_hash);
}

/** Destructor */
MultiTypedObjectInterface::~MultiTypedObjectInterface()
{
  free(data_ptr);
}
/* Methods */
/** Get obj_id value.
 * Unique and stable ID of the object.
 * @return obj_id value
 */
uint32_t
MultiTypedObjectInterface::obj_id() const
{
  return data->obj_id;
}

/** Get maximum length of obj_id value.
 * @return length of obj_id value, can be length of the array or number of 
 * maximum number of characters for a string
 */
size_t
MultiTypedObjectInterface::maxlenof_obj_id() const
{
  return 1;
}

/** Set obj_id value.
 * Unique and stable ID of the object.
 * @param new_obj_id new obj_id value
 */
void
MultiTypedObjectInterface::set_obj_id(const uint32_t new_obj_id)
{
  data->obj_id = new_obj_id;
  data_changed = true;
}

/** Get type_1 value.
 * Possible type of the object.
 * @return type_1 value
 */
char *
MultiTypedObjectInterface::type_1() const
{
  return data->type_1;
}

/** Get maximum length of type_1 value.
 * @return length of type_1 value, can be length of the array or number of 
 * maximum number of characters for a string
 */
size_t
MultiTypedObjectInterface::maxlenof_type_1() const
{
  return 32;
}

/** Set type_1 value.
 * Possible type of the object.
 * @param new_type_1 new type_1 value
 */
void
MultiTypedObjectInterface::set_type_1(const char * new_type_1)
{
  strncpy(data->type_1, new_type_1, sizeof(data->type_1));
  data_changed = true;
}

/** Get type_2 value.
 * Possible type of the object.
 * @return type_2 value
 */
char *
MultiTypedObjectInterface::type_2() const
{
  return data->type_2;
}

/** Get maximum length of type_2 value.
 * @return length of type_2 value, can be length of the array or number of 
 * maximum number of characters for a string
 */
size_t
MultiTypedObjectInterface::maxlenof_type_2() const
{
  return 32;
}

/** Set type_2 value.
 * Possible type of the object.
 * @param new_type_2 new type_2 value
 */
void
MultiTypedObjectInterface::set_type_2(const char * new_type_2)
{
  strncpy(data->type_2, new_type_2, sizeof(data->type_2));
  data_changed = true;
}

/** Get type_3 value.
 * Possible type of the object.
 * @return type_3 value
 */
char *
MultiTypedObjectInterface::type_3() const
{
  return data->type_3;
}

/** Get maximum length of type_3 value.
 * @return length of type_3 value, can be length of the array or number of 
 * maximum number of characters for a string
 */
size_t
MultiTypedObjectInterface::maxlenof_type_3() const
{
  return 32;
}

/** Set type_3 value.
 * Possible type of the object.
 * @param new_type_3 new type_3 value
 */
void
MultiTypedObjectInterface::set_type_3(const char * new_type_3)
{
  strncpy(data->type_3, new_type_3, sizeof(data->type_3));
  data_changed = true;
}

/** Get type_4 value.
 * Possible type of the object.
 * @return type_4 value
 */
char *
MultiTypedObjectInterface::type_4() const
{
  return data->type_4;
}

/** Get maximum length of type_4 value.
 * @return length of type_4 value, can be length of the array or number of 
 * maximum number of characters for a string
 */
size_t
MultiTypedObjectInterface::maxlenof_type_4() const
{
  return 32;
}

/** Set type_4 value.
 * Possible type of the object.
 * @param new_type_4 new type_4 value
 */
void
MultiTypedObjectInterface::set_type_4(const char * new_type_4)
{
  strncpy(data->type_4, new_type_4, sizeof(data->type_4));
  data_changed = true;
}

/* =========== message create =========== */
Message *
MultiTypedObjectInterface::create_message(const char *type) const
{
  throw UnknownTypeException("The given type '%s' does not match any known "
                             "message type for this interface type.", type);
}


/** Copy values from other interface.
 * @param other other interface to copy values from
 */
void
MultiTypedObjectInterface::copy_values(const Interface *other)
{
  const MultiTypedObjectInterface *oi = dynamic_cast<const MultiTypedObjectInterface *>(other);
  if (oi == NULL) {
    throw TypeMismatchException("Can only copy values from interface of same type (%s vs. %s)",
                                type(), other->type());
  }
  memcpy(data, oi->data, sizeof(MultiTypedObjectInterface_data_t));
}

const char *
MultiTypedObjectInterface::enum_tostring(const char *enumtype, int val) const
{
  throw UnknownTypeException("Unknown enum type %s", enumtype);
}

/* =========== messages =========== */
/** Check if message is valid and can be enqueued.
 * @param message Message to check
 * @return true if the message is valid, false otherwise.
 */
bool
MultiTypedObjectInterface::message_valid(const Message *message) const
{
  return false;
}

/// @cond INTERNALS
EXPORT_INTERFACE(MultiTypedObjectInterface)
/// @endcond


} // end namespace fawkes
