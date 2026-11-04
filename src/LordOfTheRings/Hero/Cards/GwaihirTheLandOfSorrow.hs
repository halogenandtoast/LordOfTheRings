module LordOfTheRings.Hero.Cards.GwaihirTheLandOfSorrow (gwaihirTheLandOfSorrow) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Eagle))

newtype GwaihirTheLandOfSorrow = GwaihirTheLandOfSorrow HeroAttrs
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, Entity)

gwaihirTheLandOfSorrow :: HeroCard GwaihirTheLandOfSorrow
gwaihirTheLandOfSorrow = hero GwaihirTheLandOfSorrow Cards.gwaihirTheLandOfSorrow

-- | Cannot have restricted attachments. Gwaihir does not ready during the refresh phase.
instance HasModifiersFor GwaihirTheLandOfSorrow where
  getModifiersFor (GwaihirTheLandOfSorrow a) = modifySelf a [CannotHaveRestrictedAttachments, DoesNotReadyDuringRefresh]

-- | Response: After an Eagle ally enters or leaves play, ready Gwaihir. (Limit once per
-- phase.) Any player's Eagle ally.
instance HasAbilities GwaihirTheLandOfSorrow where
  getAbilities (GwaihirTheLandOfSorrow a) =
    [ limited (PlayerLimit PerPhase 1)
        $ restricted a 1 (exists $ HeroWithId a.id <> ExhaustedHero)
        $ response
        $ AnyWindowOf
          [ AllyEnteredPlayWindow After Nothing (Just Eagle)
          , CharacterLeftPlayWindow After (CardWithType AllyType <> CardWithTrait Eagle)
          ]
    ]

instance RunMessage GwaihirTheLandOfSorrow where
  runMessage msg h@(GwaihirTheLandOfSorrow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    _ -> GwaihirTheLandOfSorrow <$> runMessage msg attrs
