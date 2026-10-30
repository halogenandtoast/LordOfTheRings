module LordOfTheRings.Location.Cards.SinisterDungeon (sinisterDungeon) where

import LordOfTheRings.Card (Card (PlayerCard), EncounterCard (..), toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Sorcery))

newtype SinisterDungeon = SinisterDungeon LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sinisterDungeon :: LocationCard SinisterDungeon
sinisterDungeon = location SinisterDungeon Cards.sinisterDungeon

-- | Forced: When Sinister Dungeon is explored, the first player reanimates a random card
-- from his hand and deals Sinister Dungeon to it as a shadow card.
instance HasAbilities SinisterDungeon where
  getAbilities (SinisterDungeon a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

{- | Explored, it is discarded as usual, then leaves the discard pile as the Reanimated
Dead's shadow card. With an empty hand nothing is reanimated and it stays discarded.
Shadow: Discard the top 2 cards of the encounter deck. Resolve the "when revealed"
effects of each Sorcery card discarded by this effect.
-}
instance RunMessage SinisterDungeon where
  runMessage msg l@(SinisterDungeon a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      hand <- shuffleM . (\p -> [c | PlayerCard c <- p.hand]) =<< getPlayer lead
      for_ (take 1 hand) \c -> do
        eid <- reanimate c.id
        let card = MkEncounterCard a.cardId (toCardCode a)
        cancelMatching \case
          Discard (LocationTarget lid) -> lid == a.id
          _ -> False
        discard a
        removeFromEncounterDiscard card
        whileInPlay (EnemyTarget eid) $ push $ AttachShadowCard eid card
      pure l
    ResolveShadowEffect cid _ _ _ | cid == a.cardId -> do
      top <- getsGame (take 2 . (.encounterDeck) . toAttrs . (.scenario))
      discardTopOfEncounterDeck a 2
      for_ top \card -> when (Sorcery `member` (toCardDef card).traits) $ resolveWhenRevealedFromDiscard card
      pure l
    _ -> SinisterDungeon <$> runMessage msg a
