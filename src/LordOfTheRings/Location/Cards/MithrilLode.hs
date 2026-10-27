module LordOfTheRings.Location.Cards.MithrilLode (mithrilLode) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype MithrilLode = MithrilLode LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mithrilLode :: LocationCard MithrilLode
mithrilLode = location MithrilLode Cards.mithrilLode

-- | Doomed 1.
-- While Mithril Lode is the active location, it gains: "Refresh Action: Exhaust a
-- character you control to place X progress tokens on the current quest card, bypassing
-- any active location. X is the exhausted character's willpower. (Limit once per round.)"
instance HasAbilities MithrilLode where
  getAbilities (MithrilLode a) =
    [ limited (GroupLimit PerRound 1)
        $ anyPlayer
        $ restricted a 1 (DuringPhase RefreshPhase <> CharactersAtLeast 1 (HeroControlledBy ActivePlayer <> ReadyHero) (AllyControlledBy ActivePlayer <> ReadyAlly))
        $ ActionAbility Free
    | a.placement == ActiveLocation
    ]

instance RunMessage MithrilLode where
  runMessage msg l@(MithrilLode a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      able <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
      chooseOrRunOneM pid do
        questionLabeled "mithrilLode.exhaust"
        causedBy a
        targets able \c -> do
          x <- getWillpower c
          exhaust c
          placeProgressOnCurrentQuest a x
      pure l
    _ -> MithrilLode <$> runMessage msg a
