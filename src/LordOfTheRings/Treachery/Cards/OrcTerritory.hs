module LordOfTheRings.Treachery.Cards.OrcTerritory (orcTerritory) where

import LordOfTheRings.Card (CardType (LocationType))
import LordOfTheRings.Helpers.Character (getCommittedWillpower)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype OrcTerritory = OrcTerritory TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcTerritory :: TreacheryCard OrcTerritory
orcTerritory = treachery OrcTerritory Cards.orcTerritory

-- | When Revealed: Each player searches the encounter deck and discard pile for a location
-- and adds it to the staging area. If the total threat in the staging area is less than
-- the total willpower of all characters committed to the quest, each Orc enemy engaged
-- with a player makes an immediate attack.
instance RunMessage OrcTerritory where
  runMessage msg t@(OrcTerritory attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchEncounterDeckAndDiscard p attrs (CardWithType LocationType)
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      threat <- getStagingThreat
      willpower <- getCommittedWillpower
      when (threat < willpower) do
        players <- getRemainingPlayers
        for_ players \p -> do
          orcs <- select (EnemyWithTrait Orc <> EnemyEngagedWith (PlayerWithId p))
          for_ orcs (immediateAttack p)
      pure t
    _ -> OrcTerritory <$> runMessage msg attrs
