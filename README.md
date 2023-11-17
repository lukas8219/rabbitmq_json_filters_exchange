# JSON filtering Exchange

Plugin built for routing based upon JSON fields. Route your messages using JSON filters and operators such as `$or` and `$ne`.
Roadmap: Support overriding JSON filters so you can have you own implementation.

## Supported RabbitMQ Versions

The most recent release of this plugin targets RabbitMQ 3.12.x.

## Supported Erlang/OTP Versions

Latest version of this plugin [requires Erlang 25.0 or later versions](https://www.rabbitmq.com/which-erlang.html), same as RabbitMQ 3.12.x.

## Installation

TL;DR:
```base
make

DIST_AS_EZ=yes make dist
```

Copy the .EZ file generated on `plugins` dir into your Broker plugin directory

Binary builds of this plugin from
the [Community Plugins page](https://www.rabbitmq.com/community-plugins.html).

See [Plugin Installation](https://www.rabbitmq.com/installing-plugins.html) for details
about how to install plugins that do not ship with RabbitMQ.

## Building from Source

You can build and install it like any other plugin (see
[the plugin development guide](https://www.rabbitmq.com/plugin-development.html)).

## Usage

To use the Json Filters exchange, here's an example with AMQPlib
```javascript
    const { queue: emailProcessorQueue } = await channel.assertQueue('processor:email:queue');
    const { queue: phoneProcessorQueue } = await channel.assertQueue('processor:phone:queue');
    const { queue: generalProcessorQueue } = await channel.assertQueue('processor:contact:queue');

    const { exchange } = await channel.assertExchange('processor:contact:exchange', 'json');

    channel.bindQueue(emailProcessorQueue, exchange, '', { 'x-json-filters': JSON.stringify([{ contactType: 'email' }]) });
    channel.bindQueue(phoneProcessorQueue, exchange, '', { 'x-json-filters': JSON.stringify([{ contactType: 'phone' }]) });
    channel.bindQueue(generalProcessorQueue, exchange, '', { 'x-json-filters': JSON.stringify([{ $or: [{ contactType: 'email' }, { contactType: 'phone' }] }]) });
```

## Limitations
The JSON filtering is not well suited for production yet.
- Filters are only applied at Root-level of JSON for now (It is not clear wether we shall support N-level nested filtering)
- 

### Performance Limitation
We can expect a lower performance than the Headers Exchange, since this is actually headers exchange implementation ons Steroids.

### Supported operators
- PlainObject = an JSON object to match ALL the values specified
- $or = An array of PlainObject. Will match if any of those are true
- $ne = An array of PlainObject. Will match if none of those are true

### What if I want to go into production w/ it?
It's up to you. Feel free to benchmark w/ real cases using RabbitMQ PerfTool. Enhancements are always welcome!


You can nest operators.
