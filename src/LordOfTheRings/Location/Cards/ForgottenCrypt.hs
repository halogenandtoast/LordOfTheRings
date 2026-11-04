module LordOfTheRings.Location.Cards.ForgottenCrypt (forgottenCrypt) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardTraits, cdCardType, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (isLoot)
import LordOfTheRings.Trait (Trait (Undead))

newtype ForgottenCrypt = ForgottenCrypt LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

forgottenCrypt :: LocationCard ForgottenCrypt
forgottenCrypt = location ForgottenCrypt Cards.forgottenCrypt

-- | Immune to player card effects.
instance HasModifiersFor ForgottenCrypt where
  getModifiersFor (ForgottenCrypt a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After Forgotten Crypt becomes the active location, discard cards from the top
-- of the encounter deck until an Undead enemy is discarded and add it to the staging area.
-- Then, attach a Loot objective in the encounter discard pile to that enemy as a guarded
-- objective.
instance HasAbilities ForgottenCrypt where
  getAbilities (ForgottenCrypt a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | The Loot objective may be one just discarded; the first player picks among several.
-- With no Undead enemy found, there is nothing to attach it to.
instance RunMessage ForgottenCrypt where
  runMessage msg l@(ForgottenCrypt a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardEncounterCardsUntil a (CardWithType EnemyType <> CardWithTrait Undead)
      pure l
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      let undead c = cdCardType (toCardDef c) == EnemyType && Undead `member` cdCardTraits (toCardDef c)
      for_ (find undead cards) \enemy -> do
        addToStagingFromDiscard enemy
        loot <- getsGame (filter isLoot . (.encounterDiscard) . toAttrs . (.scenario))
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "forgottenCrypt.loot"
          causedBy a
          targets loot \o -> do
            removeFromEncounterDiscard o
            putObjectiveIntoPlayGuardedBy o enemy.id
      pure l
    _ -> ForgottenCrypt <$> runMessage msg a
