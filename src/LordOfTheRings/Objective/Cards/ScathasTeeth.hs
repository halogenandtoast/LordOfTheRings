module LordOfTheRings.Objective.Cards.ScathasTeeth (scathasTeeth) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootObjective)

newtype ScathasTeeth = ScathasTeeth ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

scathasTeeth :: ObjectiveCard ScathasTeeth
scathasTeeth = objective ScathasTeeth Cards.scathasTeeth

-- | Loot. If unattached, the first player claims Scatha's Teeth and attaches it to a hero he
-- controls.
instance HasModifiersFor ScathasTeeth where
  getModifiersFor (ScathasTeeth a) = lootObjective a

-- | Action: Exhaust Scatha's Teeth and attached hero to choose a player. That player
-- reduces his threat by 2.
instance HasAbilities ScathasTeeth where
  getAbilities (ScathasTeeth a) =
    [ mkAbility a 1 $ ActionAbility (Costs [exhaustCost a, exhaustCost host])
    | AttachedTo host <- [a.placement]
    , not a.exhausted
    ]

-- | An encounter card's effect, so Moonlit Passage does not limit it.
instance RunMessage ScathasTeeth where
  runMessage msg o@(ScathasTeeth attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      chooseOrRunOneM pid do
        questionLabeled "scathasTeeth.player"
        causedBy attrs
        targets players \p -> lowerThreatBy attrs p 2
      pure o
    _ -> ScathasTeeth <$> runMessage msg attrs
