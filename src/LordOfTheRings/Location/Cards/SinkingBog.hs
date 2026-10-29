module LordOfTheRings.Location.Cards.SinkingBog (sinkingBog) where

import LordOfTheRings.Card (CardDef (cdCardTraits), HasCardDef (toCardDef), toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Item))

newtype SinkingBog = SinkingBog LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sinkingBog :: LocationCard SinkingBog
sinkingBog = location SinkingBog Cards.sinkingBog

{- | While Sinking Bog is in the staging area, each character gets -1 willpower, -1 attack
and -1 defense for each Item attached to it. This ability does not stack with other copies
of Sinking Bog: only the first copy in the staging area gives it.
-}
instance HasModifiersFor SinkingBog where
  getModifiersFor (SinkingBog a) = do
    bogs <- select (LocationInStagingArea <> LocationWithCardCode (toCardCode Cards.sinkingBog))
    when (headMay (sort bogs) == Just a.id) do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \c -> do
        items <- length . filter ((Item `member`) . cdCardTraits) <$> attachedCards c
        when (items > 0)
          $ modified a c [WillpowerModifier (negate items), AttackModifier (negate items), DefenseModifier (negate items)]

-- | Shadow: Defending character gets -1 defense for this attack for each attachment
-- attached to it.
instance RunMessage SinkingBog where
  runMessage msg l@(SinkingBog a) = case msg of
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      for_ defenders \d -> do
        n <- length <$> attachedCards d
        when (n > 0) $ attackModifier a d (DefenseModifier (negate n))
      pure l
    _ -> SinkingBog <$> runMessage msg a

-- | The cards attached to the character: attachments, conditions and claimed objectives.
attachedCards :: HasGame m => Target -> m [CardDef]
attachedCards c = getsGame \g ->
  let on' :: HasField "placement" x Placement => x -> Bool
      on' x = x.placement == AttachedTo c
      es = g.entities
   in [toCardDef x | x <- toList es.attachments, on' x]
        <> [toCardDef x | x <- toList es.treacheries, on' x]
        <> [toCardDef x | x <- toList es.objectives, on' x]
