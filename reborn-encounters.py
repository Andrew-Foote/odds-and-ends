# very niche tool: puts the encounters.txt PBS file for Pokemon Reborn into a more easily-readable form
# could probably be easily adapted other games made with Pokemon Essentials

from collections import defaultdict
from functools import partial
import io
import re
import sys


# these are defined in PokemonEncounters.rb, but we'll just hard-code the ones from Reborn for now
ENCOUNTER_TYPES = {
    'Land': [20,15,12,10,10,10,5,5,5,4,2,2],
    'Cave': [20,15,12,10,10,10,5,5,5,4,2,2],
    'Water': [50,25,15,7,3],
    'RockSmash': [50,25,15,7,3],
    'OldRod': [70,30],
    'GoodRod': [60,20,20],
    'SuperRod': [40,35,15,7,3],
    'HeadbuttLow': [30,25,20,10,5,5,4,1],
    'HeadbuttHigh': [30,25,20,10,5,5,4,1],
    'LandMorning': [20,15,12,10,10,10,5,5,5,4,2,2],
    'LandDay': [20,15,12,10,10,10,5,5,5,4,2,2],
    'LandNight': [20,15,12,10,10,10,5,5,5,4,2,2],
    'BugContest': [20,15,12,10,10,10,5,5,5,4,2,2]
}


def parse(ftext):
    state = 'expecting_hashes'
    areas = {}
    current_area_code = None
    lineno = 0
    lines = ftext.splitlines()
    #print(f'number of lines: {len(lines)}')
    #input()

    while lineno < len(lines):
        line = lines[lineno]
        #print(' / '.join([str(lineno), str(state), line]))
        #input()

        if state == 'expecting_hashes':
            if re.match(r'#+', line) is None: 
                raise ValueError(f'line {lineno}: expecting hashes, instead got:\n{line}')

            lineno += 1
            state = 'expecting_area_info'

        elif state == 'expecting_area_info':
            parts = re.split(r'\s*#\s*', line)
            current_area_code = int(parts[0])
            area_name = parts[1]
            areas[current_area_code] = {'area_name': area_name}
            lineno += 1
            state = 'expecting_densities'

        elif state == 'expecting_densities':
            current_area = areas[current_area_code]

            if re.match(r'\d+,\d+,\d+', line) is None:
                # this is an optional line, default values are 25,10,10
                # (grass, cave, surf respectively)
                current_area['densities'] = [25, 10, 10]
                state = 'expecting_encounter_type'
            else:
                current_area['densities'] = map(int, line.split(','))
                lineno += 1
                state = 'expecting_encounter_type'

        elif state == 'expecting_encounter_type':
            current_area = areas[current_area_code]

            if re.match(r'#+', line) is not None:
                # this area's done
                lineno += 1
                state = 'expecting_area_info'

            elif line not in ENCOUNTER_TYPES:
                raise ValueError(f'line {lineno}: expecting hashes or encounter type, instead got:\n{line}')

            else:
                if 'encounter_types' not in current_area:
                    current_area['encounter_types'] = {}

                if line in current_area['encounter_types']:
                    raise ValueError(f'line {lineno}: {line} encounters are defined more than once')

                current_area['encounter_types'][line] = []
                lineno += 1
                state = ['expecting_pokemon', line, 0]

        elif isinstance(state, list) and state[0] == 'expecting_pokemon':
            current_area = areas[current_area_code]
            encounter_type = state[1]
            slot_number = state[2]

            if slot_number < len(ENCOUNTER_TYPES[encounter_type]):
                if re.match(r'\w+,\d+(?:,\d+)?', line) is None:
                    raise ValueError(f'line {lineno}: expecting pokemon for slot {slot_number} in {encounter_type}, instead got:\n{line}')
                parts = line.split(',')
                pokemon = parts[0]
                min_level = int(parts[1])
                max_level = int(parts[2]) if len(parts) >= 3 else min_level
                current_area['encounter_types'][encounter_type].append([pokemon, min_level, max_level])
                lineno += 1
                state = ['expecting_pokemon', encounter_type, slot_number + 1]
            else:
                state = 'expecting_encounter_type'

    return areas


def density_to_percentage(density):
    # check PokemonEncounters.rb (pbGenerateEncounter, which is called per step)
    # to make sure, but for Reborn it multiples the encounter density by 16
    # and then rolls a random integer between 0 and 250 * 16 - 1 (inclusive)
    # so the density is effectively the encounter probability multiplied by 250
    # hence, to convert it to a percentage, we divide by 250 and multiply by 100
    return (density / 250) * 100


def format(areas):
    with io.StringIO() as res:
        printres = partial(print, file=res)

        for area_code, area in areas.items():
            area_name = area['area_name']
            densities = list(map(density_to_percentage, area['densities']))

            printres(f'[{area_code}] {area_name}')
            printres(f'Base encounter rates per step: {densities[0]:.2f} (grass), {densities[1]:.2f} (cave), {densities[2]:.2f} (surf)')

            for enctype, encounters in area['encounter_types'].items():
                printres(f'[{enctype}]')
                chances = ENCOUNTER_TYPES[enctype]
                chances_total = sum(chances)

                # add per-slot probabilities
                for slot_number, encounter in enumerate(encounters):
                    encounter.append(chances[slot_number])

                # group by pokemon
                groups = defaultdict(lambda: [])
                
                #print(encounters)
                #input()

                for pokemon, min_level, max_level, rate in encounters:
                    groups[pokemon].append((min_level, max_level, rate))

                # print(dict(groups))
                # input()

                levelwise_groups = defaultdict(lambda: defaultdict(lambda: 0))

                for pokemon, group in groups.items():
                    for min_level, max_level, rate in group:
                        number_of_levels = (max_level + 1) - min_level

                        for level in range(min_level, max_level + 1):
                            levelwise_groups[pokemon][level] += rate / number_of_levels

                # print(dict(levelwise_groups))
                # input()

                levelgrouped_groups = defaultdict(lambda: [])

                for pokemon, group in levelwise_groups.items():
                    levels = sorted(group.keys())
                    levelgrouped_group = levelgrouped_groups[pokemon]
                    current_line = [levels[0], levels[0], group[levels[0]]]

                    for level in levels[1:]:
                        if level == current_line[1] + 1 and group[level] == current_line[2] / ((current_line[1] + 1) - current_line[0]):
                            current_line[1] += 1
                            current_line[2] += group[level]
                        else:
                            levelgrouped_group.append(current_line)
                            current_line = [level, level, group[level]]

                    levelgrouped_group.append(current_line)

                # print(dict(levelgrouped_groups))
                # input()

                simplified_groups = []

                for pokemon, group in levelgrouped_groups.items():
                    level_lines = []

                    for min_level, max_level, rate in group:
                        level_lines.append([min_level, max_level, rate])

                    level_lines.sort(key=lambda line: -line[2])
                    simplified_groups.append([pokemon, level_lines])

                simplified_groups.sort(key=lambda line: -sum(rate for min_level, max_level, rate in line[1]))

                for pokemon, level_lines in simplified_groups:
                    total_rate = sum(rate for min_level, max_level, rate in level_lines)
                    printres(f'  {total_rate:0>5.2f}% {pokemon}', end=' ')
                    level_bits = []

                    for min_level, max_level, rate in level_lines:
                        level_string = f'lv. {min_level}' if max_level == min_level else f'lv. {min_level} -- {max_level}'
                        level_bits.append(f'{rate:0>5.2f}% {level_string}')

                    printres('[' + ', '.join(level_bits) + ']')

            printres()

        return res.getvalue()


if __name__ == '__main__':
    ifname = sys.argv[1]
    ofname = sys.argv[2]

    with open(ifname, 'r') as f:
        iftext = f.read()

    data = parse(iftext)
    print('parsed')
    formatted = format(data)
    print('formatted')

    with open(ofname, 'w') as f:
        f.write(formatted)

    print("done")