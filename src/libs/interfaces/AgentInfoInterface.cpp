
/***************************************************************************
 *  AgentInfoInterface.cpp - Fawkes BlackBoard Interface - AgentInfoInterface
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

#include <interfaces/AgentInfoInterface.h>

#include <core/exceptions/software.h>

#include <cstring>
#include <cstdlib>

namespace fawkes {

/** @class AgentInfoInterface <interfaces/AgentInfoInterface.h>
 * AgentInfoInterface Fawkes BlackBoard Interface.
 * 
      Informational messages that the agent may send for visualization.
    
 * @ingroup FawkesInterfaces
 */



/** Constructor */
AgentInfoInterface::AgentInfoInterface() : Interface()
{
  data_size = sizeof(AgentInfoInterface_data_t);
  data_ptr  = malloc(data_size);
  data      = (AgentInfoInterface_data_t *)data_ptr;
  data_ts   = (interface_data_ts_t *)data_ptr;
  memset(data_ptr, 0, data_size);
  add_fieldinfo(IFT_STRING, "message", 256, data->message);
  unsigned char tmp_hash[] = {0xb3, 0x36, 0xde, 0xf8, 0x3, 0x8b, 0xfa, 0x6e, 0x84, 0x2a, 0xc5, 0xca, 0x6a, 0xb6, 0xb7, 0x8f};
  set_hash(tmp_hash);
}

/** Destructor */
AgentInfoInterface::~AgentInfoInterface()
{
  free(data_ptr);
}
/* Methods */
/** Get message value.
 * Info message.
 * @return message value
 */
char *
AgentInfoInterface::message() const
{
  return data->message;
}

/** Get maximum length of message value.
 * @return length of message value, can be length of the array or number of 
 * maximum number of characters for a string
 */
size_t
AgentInfoInterface::maxlenof_message() const
{
  return 256;
}

/** Set message value.
 * Info message.
 * @param new_message new message value
 */
void
AgentInfoInterface::set_message(const char * new_message)
{
  strncpy(data->message, new_message, sizeof(data->message));
  data_changed = true;
}

/* =========== message create =========== */
Message *
AgentInfoInterface::create_message(const char *type) const
{
  throw UnknownTypeException("The given type '%s' does not match any known "
                             "message type for this interface type.", type);
}


/** Copy values from other interface.
 * @param other other interface to copy values from
 */
void
AgentInfoInterface::copy_values(const Interface *other)
{
  const AgentInfoInterface *oi = dynamic_cast<const AgentInfoInterface *>(other);
  if (oi == NULL) {
    throw TypeMismatchException("Can only copy values from interface of same type (%s vs. %s)",
                                type(), other->type());
  }
  memcpy(data, oi->data, sizeof(AgentInfoInterface_data_t));
}

const char *
AgentInfoInterface::enum_tostring(const char *enumtype, int val) const
{
  throw UnknownTypeException("Unknown enum type %s", enumtype);
}

/* =========== messages =========== */
/** Check if message is valid and can be enqueued.
 * @param message Message to check
 * @return true if the message is valid, false otherwise.
 */
bool
AgentInfoInterface::message_valid(const Message *message) const
{
  return false;
}

/// @cond INTERNALS
EXPORT_INTERFACE(AgentInfoInterface)
/// @endcond


} // end namespace fawkes
