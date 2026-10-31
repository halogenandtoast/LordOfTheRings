module LordOfTheRings.Quest.Cards.RescueFromTheTrolls (rescueFromTheTrolls) where

import LordOfTheRings.Card (Card (PlayerCard), CardDef (cdThreatCost), CardType (HeroType), cdCardType, toCard, toCardDef)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Troll))

newtype RescueFromTheTrolls = RescueFromTheTrolls QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
rescueFromTheTrolls :: QuestCard RescueFromTheTrolls
rescueFromTheTrolls = quest RescueFromTheTrolls Cards.rescueFromTheTrolls

-- | Response: After a Troll enemy is destroyed, place 3 progress on Rescue from the Trolls.
instance HasAbilities RescueFromTheTrolls where
  getAbilities (RescueFromTheTrolls a) = [mkAbility a 1 $ response $ EnemyDestroyedWindow After (CardWithTrait Troll)]

-- | When Revealed: Each player places the lowest-threat-cost hero he controls facedown
-- underneath Rescue from the Trolls, out of play.
-- Forced: When Rescue from the Trolls is defeated, each player takes control of each of his
-- heroes underneath it.
instance RunMessage RescueFromTheTrolls where
  runMessage msg q@(RescueFromTheTrolls attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId p)
        let cost = fromMaybe 0 . cdThreatCost . toCardDef
            lowest = minimumMay (map cost heroes)
        chooseOrRunOneM p do
          questionLabeled "rescueFromTheTrolls.hero"
          causedBy attrs
          targets [toId h | h <- heroes, Just (cost h) == lowest] \hid ->
            for_ (find ((== hid) . toId) heroes) \h -> placeUnderneath attrs [toCard h]
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgress attrs attrs 3
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      for_ [pc | PlayerCard pc <- attrs.underneath, cdCardType (toCardDef pc) == HeroType] \pc ->
        for_ pc.owner \owner -> putHeroIntoPlay owner pc
      RescueFromTheTrolls <$> runMessage msg attrs
    _ -> RescueFromTheTrolls <$> runMessage msg attrs
