events_list = [[], [], []]
df_list = list()
for events in events_list:
    for event in events:
        df_list.append([event.timestamp, event.value, event.name, event.desc])
