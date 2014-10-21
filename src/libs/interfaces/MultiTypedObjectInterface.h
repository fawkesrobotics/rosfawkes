
/***************************************************************************
 *  MultiTypedObjectInterface.h - Fawkes BlackBoard Interface - MultiTypedObjectInterface
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

#ifndef __INTERFACES_MULTITYPEDOBJECTINTERFACE_H_
#define __INTERFACES_MULTITYPEDOBJECTINTERFACE_H_

#include <interface/interface.h>
#include <interface/message.h>
#include <interface/field_iterator.h>

namespace fawkes {

class MultiTypedObjectInterface : public Interface
{
 /// @cond INTERNALS
 INTERFACE_MGMT_FRIENDS(MultiTypedObjectInterface)
 /// @endcond
 public:
  /* constants */

 private:
#pragma pack(push,4)
  /** Internal data storage, do NOT modify! */
  typedef struct {
    int64_t timestamp_sec;  /**< Interface Unix timestamp, seconds */
    int64_t timestamp_usec; /**< Interface Unix timestamp, micro-seconds */
    uint32_t obj_id; /**< Unique and stable ID of the object. */
    char type_1[32]; /**< Possible type of the object. */
    char type_2[32]; /**< Possible type of the object. */
    char type_3[32]; /**< Possible type of the object. */
    char type_4[32]; /**< Possible type of the object. */
  } MultiTypedObjectInterface_data_t;
#pragma pack(pop)

  MultiTypedObjectInterface_data_t *data;

 public:
  /* messages */
  virtual bool message_valid(const Message *message) const;
 private:
  MultiTypedObjectInterface();
  ~MultiTypedObjectInterface();

 public:
  /* Methods */
  uint32_t obj_id() const;
  void set_obj_id(const uint32_t new_obj_id);
  size_t maxlenof_obj_id() const;
  char * type_1() const;
  void set_type_1(const char * new_type_1);
  size_t maxlenof_type_1() const;
  char * type_2() const;
  void set_type_2(const char * new_type_2);
  size_t maxlenof_type_2() const;
  char * type_3() const;
  void set_type_3(const char * new_type_3);
  size_t maxlenof_type_3() const;
  char * type_4() const;
  void set_type_4(const char * new_type_4);
  size_t maxlenof_type_4() const;
  virtual Message * create_message(const char *type) const;

  virtual void copy_values(const Interface *other);
  virtual const char * enum_tostring(const char *enumtype, int val) const;

};

} // end namespace fawkes

#endif
