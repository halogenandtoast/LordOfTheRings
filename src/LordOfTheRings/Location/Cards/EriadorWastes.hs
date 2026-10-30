module LordOfTheRings.Location.Cards.EriadorWastes (eriadorWastes) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Warg))

newtype EriadorWastes = EriadorWastes LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

eriadorWastes :: LocationCard EriadorWastes
eriadorWastes = location EriadorWastes Cards.eriadorWastes

-- | While Eriador Wastes is in the staging area, no more than 5 progress can be placed on
-- the current quest each round.
-- With no Warg enemy in the staging area to engage, its Travel cost cannot be paid.
instance HasModifiersFor EriadorWastes where
  getModifiersFor (EriadorWastes a) = do
    when (a.placement == StagingArea) do
      getCurrentQuest >>= traverse_ \q -> modified a q [ProgressPerRoundLimit 5]
    wargs <- selectAny stagedWarg
    unless wargs $ modified a a [CannotBeTraveledTo]

-- | Travel: The first player engages a Warg enemy in the staging area and deals it a
-- shadow card.
instance RunMessage EriadorWastes where
  runMessage msg (EriadorWastes a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      wargs <- select stagedWarg
      chooseOrRunOneM lead do
        questionLabeled "eriadorWastes.engage"
        causedBy a
        targets wargs \w -> do
          engageEnemy lead w
          dealShadowCard w
      EriadorWastes <$> runMessage msg a
    _ -> EriadorWastes <$> runMessage msg a

stagedWarg :: EnemyMatcher
stagedWarg = EnemyInStagingArea <> EnemyWithTrait Warg
