module LordOfTheRings.Location.Cards.GiantWeb (giantWeb) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonOn, poisonedCharacters, unconsciousCharacters)

newtype GiantWeb = GiantWeb LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

giantWeb :: LocationCard GiantWeb
giantWeb = location GiantWeb Cards.giantWeb

-- | While Giant Web is the active location, each character gets -1 willpower, -1 attack,
-- and -1 defense for each poison it has.
instance HasModifiersFor GiantWeb where
  getModifiersFor (GiantWeb a) = when (a.placement == ActiveLocation) do
    poisoned <- poisonedCharacters
    for_ poisoned \c -> do
      n <- poisonOn c
      modified a c [WillpowerModifier (-n), AttackModifier (-n), DefenseModifier (-n)]

-- | Response: After Giant Web leaves play as an explored location, ready an unconscious
-- character.
instance HasAbilities GiantWeb where
  getAbilities (GiantWeb a) =
    [ restricted a 1 (AnyCriteria [exists UnconsciousHero, exists UnconsciousAlly])
        $ response
        $ ExploredWindow When a.id
    ]

instance RunMessage GiantWeb where
  runMessage msg l@(GiantWeb a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      characters <- unconsciousCharacters
      chooseOrRunOneM pid do
        questionLabeled "giantWeb.ready"
        causedBy a
        targets characters readyUnconscious
      pure l
    _ -> GiantWeb <$> runMessage msg a
